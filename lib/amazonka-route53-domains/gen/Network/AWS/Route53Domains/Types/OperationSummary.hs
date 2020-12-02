{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.OperationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.OperationSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53Domains.Types.OperationStatus
import Network.AWS.Route53Domains.Types.OperationType

-- | OperationSummary includes the following elements.
--
--
--
-- /See:/ 'operationSummary' smart constructor.
data OperationSummary = OperationSummary'
  { _osOperationId :: !Text,
    _osStatus :: !OperationStatus,
    _osType :: !OperationType,
    _osSubmittedDate :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OperationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osOperationId' - Identifier returned to track the requested action.
--
-- * 'osStatus' - The current status of the requested operation in the system.
--
-- * 'osType' - Type of the action requested.
--
-- * 'osSubmittedDate' - The date when the request was submitted.
operationSummary ::
  -- | 'osOperationId'
  Text ->
  -- | 'osStatus'
  OperationStatus ->
  -- | 'osType'
  OperationType ->
  -- | 'osSubmittedDate'
  UTCTime ->
  OperationSummary
operationSummary pOperationId_ pStatus_ pType_ pSubmittedDate_ =
  OperationSummary'
    { _osOperationId = pOperationId_,
      _osStatus = pStatus_,
      _osType = pType_,
      _osSubmittedDate = _Time # pSubmittedDate_
    }

-- | Identifier returned to track the requested action.
osOperationId :: Lens' OperationSummary Text
osOperationId = lens _osOperationId (\s a -> s {_osOperationId = a})

-- | The current status of the requested operation in the system.
osStatus :: Lens' OperationSummary OperationStatus
osStatus = lens _osStatus (\s a -> s {_osStatus = a})

-- | Type of the action requested.
osType :: Lens' OperationSummary OperationType
osType = lens _osType (\s a -> s {_osType = a})

-- | The date when the request was submitted.
osSubmittedDate :: Lens' OperationSummary UTCTime
osSubmittedDate = lens _osSubmittedDate (\s a -> s {_osSubmittedDate = a}) . _Time

instance FromJSON OperationSummary where
  parseJSON =
    withObject
      "OperationSummary"
      ( \x ->
          OperationSummary'
            <$> (x .: "OperationId")
            <*> (x .: "Status")
            <*> (x .: "Type")
            <*> (x .: "SubmittedDate")
      )

instance Hashable OperationSummary

instance NFData OperationSummary
