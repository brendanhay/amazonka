{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateUsageLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a usage limit for a specified Amazon Redshift feature on a cluster. The usage limit is identified by the returned usage limit identifier.
module Network.AWS.Redshift.CreateUsageLimit
  ( -- * Creating a Request
    createUsageLimit,
    CreateUsageLimit,

    -- * Request Lenses
    culPeriod,
    culBreachAction,
    culTags,
    culClusterIdentifier,
    culFeatureType,
    culLimitType,
    culAmount,

    -- * Destructuring the Response
    usageLimit,
    UsageLimit,

    -- * Response Lenses
    ulAmount,
    ulLimitType,
    ulUsageLimitId,
    ulPeriod,
    ulClusterIdentifier,
    ulBreachAction,
    ulFeatureType,
    ulTags,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createUsageLimit' smart constructor.
data CreateUsageLimit = CreateUsageLimit'
  { _culPeriod ::
      !(Maybe UsageLimitPeriod),
    _culBreachAction :: !(Maybe UsageLimitBreachAction),
    _culTags :: !(Maybe [Tag]),
    _culClusterIdentifier :: !Text,
    _culFeatureType :: !UsageLimitFeatureType,
    _culLimitType :: !UsageLimitLimitType,
    _culAmount :: !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateUsageLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'culPeriod' - The time period that the amount applies to. A @weekly@ period begins on Sunday. The default is @monthly@ .
--
-- * 'culBreachAction' - The action that Amazon Redshift takes when the limit is reached. The default is log. For more information about this parameter, see 'UsageLimit' .
--
-- * 'culTags' - A list of tag instances.
--
-- * 'culClusterIdentifier' - The identifier of the cluster that you want to limit usage.
--
-- * 'culFeatureType' - The Amazon Redshift feature that you want to limit.
--
-- * 'culLimitType' - The type of limit. Depending on the feature type, this can be based on a time duration or data size. If @FeatureType@ is @spectrum@ , then @LimitType@ must be @data-scanned@ . If @FeatureType@ is @concurrency-scaling@ , then @LimitType@ must be @time@ .
--
-- * 'culAmount' - The limit amount. If time-based, this amount is in minutes. If data-based, this amount is in terabytes (TB). The value must be a positive number.
createUsageLimit ::
  -- | 'culClusterIdentifier'
  Text ->
  -- | 'culFeatureType'
  UsageLimitFeatureType ->
  -- | 'culLimitType'
  UsageLimitLimitType ->
  -- | 'culAmount'
  Integer ->
  CreateUsageLimit
createUsageLimit
  pClusterIdentifier_
  pFeatureType_
  pLimitType_
  pAmount_ =
    CreateUsageLimit'
      { _culPeriod = Nothing,
        _culBreachAction = Nothing,
        _culTags = Nothing,
        _culClusterIdentifier = pClusterIdentifier_,
        _culFeatureType = pFeatureType_,
        _culLimitType = pLimitType_,
        _culAmount = pAmount_
      }

-- | The time period that the amount applies to. A @weekly@ period begins on Sunday. The default is @monthly@ .
culPeriod :: Lens' CreateUsageLimit (Maybe UsageLimitPeriod)
culPeriod = lens _culPeriod (\s a -> s {_culPeriod = a})

-- | The action that Amazon Redshift takes when the limit is reached. The default is log. For more information about this parameter, see 'UsageLimit' .
culBreachAction :: Lens' CreateUsageLimit (Maybe UsageLimitBreachAction)
culBreachAction = lens _culBreachAction (\s a -> s {_culBreachAction = a})

-- | A list of tag instances.
culTags :: Lens' CreateUsageLimit [Tag]
culTags = lens _culTags (\s a -> s {_culTags = a}) . _Default . _Coerce

-- | The identifier of the cluster that you want to limit usage.
culClusterIdentifier :: Lens' CreateUsageLimit Text
culClusterIdentifier = lens _culClusterIdentifier (\s a -> s {_culClusterIdentifier = a})

-- | The Amazon Redshift feature that you want to limit.
culFeatureType :: Lens' CreateUsageLimit UsageLimitFeatureType
culFeatureType = lens _culFeatureType (\s a -> s {_culFeatureType = a})

-- | The type of limit. Depending on the feature type, this can be based on a time duration or data size. If @FeatureType@ is @spectrum@ , then @LimitType@ must be @data-scanned@ . If @FeatureType@ is @concurrency-scaling@ , then @LimitType@ must be @time@ .
culLimitType :: Lens' CreateUsageLimit UsageLimitLimitType
culLimitType = lens _culLimitType (\s a -> s {_culLimitType = a})

-- | The limit amount. If time-based, this amount is in minutes. If data-based, this amount is in terabytes (TB). The value must be a positive number.
culAmount :: Lens' CreateUsageLimit Integer
culAmount = lens _culAmount (\s a -> s {_culAmount = a})

instance AWSRequest CreateUsageLimit where
  type Rs CreateUsageLimit = UsageLimit
  request = postQuery redshift
  response =
    receiveXMLWrapper
      "CreateUsageLimitResult"
      (\s h x -> parseXML x)

instance Hashable CreateUsageLimit

instance NFData CreateUsageLimit

instance ToHeaders CreateUsageLimit where
  toHeaders = const mempty

instance ToPath CreateUsageLimit where
  toPath = const "/"

instance ToQuery CreateUsageLimit where
  toQuery CreateUsageLimit' {..} =
    mconcat
      [ "Action" =: ("CreateUsageLimit" :: ByteString),
        "Version" =: ("2012-12-01" :: ByteString),
        "Period" =: _culPeriod,
        "BreachAction" =: _culBreachAction,
        "Tags" =: toQuery (toQueryList "Tag" <$> _culTags),
        "ClusterIdentifier" =: _culClusterIdentifier,
        "FeatureType" =: _culFeatureType,
        "LimitType" =: _culLimitType,
        "Amount" =: _culAmount
      ]
