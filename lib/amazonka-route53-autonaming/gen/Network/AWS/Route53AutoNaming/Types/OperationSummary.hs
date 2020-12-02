{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.OperationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.OperationSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53AutoNaming.Types.OperationStatus

-- | A complex type that contains information about an operation that matches the criteria that you specified in a <https://docs.aws.amazon.com/cloud-map/latest/api/API_ListOperations.html ListOperations> request.
--
--
--
-- /See:/ 'operationSummary' smart constructor.
data OperationSummary = OperationSummary'
  { _osStatus ::
      !(Maybe OperationStatus),
    _osId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OperationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osStatus' - The status of the operation. Values include the following:     * __SUBMITTED__ : This is the initial state immediately after you submit a request.     * __PENDING__ : AWS Cloud Map is performing the operation.     * __SUCCESS__ : The operation succeeded.     * __FAIL__ : The operation failed. For the failure reason, see @ErrorMessage@ .
--
-- * 'osId' - The ID for an operation.
operationSummary ::
  OperationSummary
operationSummary =
  OperationSummary' {_osStatus = Nothing, _osId = Nothing}

-- | The status of the operation. Values include the following:     * __SUBMITTED__ : This is the initial state immediately after you submit a request.     * __PENDING__ : AWS Cloud Map is performing the operation.     * __SUCCESS__ : The operation succeeded.     * __FAIL__ : The operation failed. For the failure reason, see @ErrorMessage@ .
osStatus :: Lens' OperationSummary (Maybe OperationStatus)
osStatus = lens _osStatus (\s a -> s {_osStatus = a})

-- | The ID for an operation.
osId :: Lens' OperationSummary (Maybe Text)
osId = lens _osId (\s a -> s {_osId = a})

instance FromJSON OperationSummary where
  parseJSON =
    withObject
      "OperationSummary"
      (\x -> OperationSummary' <$> (x .:? "Status") <*> (x .:? "Id"))

instance Hashable OperationSummary

instance NFData OperationSummary
