{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes criteria to restrict the results when listing platform versions.
--
--
-- The filter is evaluated as follows: @Type Operator Values[1]@
--
--
-- /See:/ 'platformFilter' smart constructor.
data PlatformFilter = PlatformFilter'
  { _pfValues :: !(Maybe [Text]),
    _pfOperator :: !(Maybe Text),
    _pfType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PlatformFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfValues' - The list of values applied to the filtering platform version attribute. Only one value is supported for all current operators. The following list shows valid filter values for some filter attributes.     * @PlatformStatus@ : @Creating@ | @Failed@ | @Ready@ | @Deleting@ | @Deleted@      * @PlatformLifecycleState@ : @recommended@      * @SupportedTier@ : @WebServer/Standard@ | @Worker/SQS/HTTP@      * @SupportedAddon@ : @Log/S3@ | @Monitoring/Healthd@ | @WorkerDaemon/SQSD@
--
-- * 'pfOperator' - The operator to apply to the @Type@ with each of the @Values@ . Valid values: @=@ | @!=@ | @<@ | @<=@ | @>@ | @>=@ | @contains@ | @begins_with@ | @ends_with@
--
-- * 'pfType' - The platform version attribute to which the filter values are applied. Valid values: @PlatformName@ | @PlatformVersion@ | @PlatformStatus@ | @PlatformBranchName@ | @PlatformLifecycleState@ | @PlatformOwner@ | @SupportedTier@ | @SupportedAddon@ | @ProgrammingLanguageName@ | @OperatingSystemName@
platformFilter ::
  PlatformFilter
platformFilter =
  PlatformFilter'
    { _pfValues = Nothing,
      _pfOperator = Nothing,
      _pfType = Nothing
    }

-- | The list of values applied to the filtering platform version attribute. Only one value is supported for all current operators. The following list shows valid filter values for some filter attributes.     * @PlatformStatus@ : @Creating@ | @Failed@ | @Ready@ | @Deleting@ | @Deleted@      * @PlatformLifecycleState@ : @recommended@      * @SupportedTier@ : @WebServer/Standard@ | @Worker/SQS/HTTP@      * @SupportedAddon@ : @Log/S3@ | @Monitoring/Healthd@ | @WorkerDaemon/SQSD@
pfValues :: Lens' PlatformFilter [Text]
pfValues = lens _pfValues (\s a -> s {_pfValues = a}) . _Default . _Coerce

-- | The operator to apply to the @Type@ with each of the @Values@ . Valid values: @=@ | @!=@ | @<@ | @<=@ | @>@ | @>=@ | @contains@ | @begins_with@ | @ends_with@
pfOperator :: Lens' PlatformFilter (Maybe Text)
pfOperator = lens _pfOperator (\s a -> s {_pfOperator = a})

-- | The platform version attribute to which the filter values are applied. Valid values: @PlatformName@ | @PlatformVersion@ | @PlatformStatus@ | @PlatformBranchName@ | @PlatformLifecycleState@ | @PlatformOwner@ | @SupportedTier@ | @SupportedAddon@ | @ProgrammingLanguageName@ | @OperatingSystemName@
pfType :: Lens' PlatformFilter (Maybe Text)
pfType = lens _pfType (\s a -> s {_pfType = a})

instance Hashable PlatformFilter

instance NFData PlatformFilter

instance ToQuery PlatformFilter where
  toQuery PlatformFilter' {..} =
    mconcat
      [ "Values" =: toQuery (toQueryList "member" <$> _pfValues),
        "Operator" =: _pfOperator,
        "Type" =: _pfType
      ]
