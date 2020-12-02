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
-- Module      : Network.AWS.Redshift.ModifyUsageLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a usage limit in a cluster. You can't modify the feature type or period of a usage limit.
module Network.AWS.Redshift.ModifyUsageLimit
  ( -- * Creating a Request
    modifyUsageLimit,
    ModifyUsageLimit,

    -- * Request Lenses
    mulAmount,
    mulBreachAction,
    mulUsageLimitId,

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

-- | /See:/ 'modifyUsageLimit' smart constructor.
data ModifyUsageLimit = ModifyUsageLimit'
  { _mulAmount ::
      !(Maybe Integer),
    _mulBreachAction :: !(Maybe UsageLimitBreachAction),
    _mulUsageLimitId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyUsageLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mulAmount' - The new limit amount. For more information about this parameter, see 'UsageLimit' .
--
-- * 'mulBreachAction' - The new action that Amazon Redshift takes when the limit is reached. For more information about this parameter, see 'UsageLimit' .
--
-- * 'mulUsageLimitId' - The identifier of the usage limit to modify.
modifyUsageLimit ::
  -- | 'mulUsageLimitId'
  Text ->
  ModifyUsageLimit
modifyUsageLimit pUsageLimitId_ =
  ModifyUsageLimit'
    { _mulAmount = Nothing,
      _mulBreachAction = Nothing,
      _mulUsageLimitId = pUsageLimitId_
    }

-- | The new limit amount. For more information about this parameter, see 'UsageLimit' .
mulAmount :: Lens' ModifyUsageLimit (Maybe Integer)
mulAmount = lens _mulAmount (\s a -> s {_mulAmount = a})

-- | The new action that Amazon Redshift takes when the limit is reached. For more information about this parameter, see 'UsageLimit' .
mulBreachAction :: Lens' ModifyUsageLimit (Maybe UsageLimitBreachAction)
mulBreachAction = lens _mulBreachAction (\s a -> s {_mulBreachAction = a})

-- | The identifier of the usage limit to modify.
mulUsageLimitId :: Lens' ModifyUsageLimit Text
mulUsageLimitId = lens _mulUsageLimitId (\s a -> s {_mulUsageLimitId = a})

instance AWSRequest ModifyUsageLimit where
  type Rs ModifyUsageLimit = UsageLimit
  request = postQuery redshift
  response =
    receiveXMLWrapper
      "ModifyUsageLimitResult"
      (\s h x -> parseXML x)

instance Hashable ModifyUsageLimit

instance NFData ModifyUsageLimit

instance ToHeaders ModifyUsageLimit where
  toHeaders = const mempty

instance ToPath ModifyUsageLimit where
  toPath = const "/"

instance ToQuery ModifyUsageLimit where
  toQuery ModifyUsageLimit' {..} =
    mconcat
      [ "Action" =: ("ModifyUsageLimit" :: ByteString),
        "Version" =: ("2012-12-01" :: ByteString),
        "Amount" =: _mulAmount,
        "BreachAction" =: _mulBreachAction,
        "UsageLimitId" =: _mulUsageLimitId
      ]
