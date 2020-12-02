{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditCheckDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditCheckDetails where

import Network.AWS.IoT.Types.AuditCheckRunStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the audit check.
--
--
--
-- /See:/ 'auditCheckDetails' smart constructor.
data AuditCheckDetails = AuditCheckDetails'
  { _acdSuppressedNonCompliantResourcesCount ::
      !(Maybe Integer),
    _acdTotalResourcesCount :: !(Maybe Integer),
    _acdCheckCompliant :: !(Maybe Bool),
    _acdNonCompliantResourcesCount :: !(Maybe Integer),
    _acdErrorCode :: !(Maybe Text),
    _acdMessage :: !(Maybe Text),
    _acdCheckRunStatus :: !(Maybe AuditCheckRunStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AuditCheckDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acdSuppressedNonCompliantResourcesCount' - Describes how many of the non-compliant resources created during the evaluation of an audit check were marked as suppressed.
--
-- * 'acdTotalResourcesCount' - The number of resources on which the check was performed.
--
-- * 'acdCheckCompliant' - True if the check is complete and found all resources compliant.
--
-- * 'acdNonCompliantResourcesCount' - The number of resources that were found noncompliant during the check.
--
-- * 'acdErrorCode' - The code of any error encountered when this check is performed during this audit. One of "INSUFFICIENT_PERMISSIONS" or "AUDIT_CHECK_DISABLED".
--
-- * 'acdMessage' - The message associated with any error encountered when this check is performed during this audit.
--
-- * 'acdCheckRunStatus' - The completion status of this check. One of "IN_PROGRESS", "WAITING_FOR_DATA_COLLECTION", "CANCELED", "COMPLETED_COMPLIANT", "COMPLETED_NON_COMPLIANT", or "FAILED".
auditCheckDetails ::
  AuditCheckDetails
auditCheckDetails =
  AuditCheckDetails'
    { _acdSuppressedNonCompliantResourcesCount =
        Nothing,
      _acdTotalResourcesCount = Nothing,
      _acdCheckCompliant = Nothing,
      _acdNonCompliantResourcesCount = Nothing,
      _acdErrorCode = Nothing,
      _acdMessage = Nothing,
      _acdCheckRunStatus = Nothing
    }

-- | Describes how many of the non-compliant resources created during the evaluation of an audit check were marked as suppressed.
acdSuppressedNonCompliantResourcesCount :: Lens' AuditCheckDetails (Maybe Integer)
acdSuppressedNonCompliantResourcesCount = lens _acdSuppressedNonCompliantResourcesCount (\s a -> s {_acdSuppressedNonCompliantResourcesCount = a})

-- | The number of resources on which the check was performed.
acdTotalResourcesCount :: Lens' AuditCheckDetails (Maybe Integer)
acdTotalResourcesCount = lens _acdTotalResourcesCount (\s a -> s {_acdTotalResourcesCount = a})

-- | True if the check is complete and found all resources compliant.
acdCheckCompliant :: Lens' AuditCheckDetails (Maybe Bool)
acdCheckCompliant = lens _acdCheckCompliant (\s a -> s {_acdCheckCompliant = a})

-- | The number of resources that were found noncompliant during the check.
acdNonCompliantResourcesCount :: Lens' AuditCheckDetails (Maybe Integer)
acdNonCompliantResourcesCount = lens _acdNonCompliantResourcesCount (\s a -> s {_acdNonCompliantResourcesCount = a})

-- | The code of any error encountered when this check is performed during this audit. One of "INSUFFICIENT_PERMISSIONS" or "AUDIT_CHECK_DISABLED".
acdErrorCode :: Lens' AuditCheckDetails (Maybe Text)
acdErrorCode = lens _acdErrorCode (\s a -> s {_acdErrorCode = a})

-- | The message associated with any error encountered when this check is performed during this audit.
acdMessage :: Lens' AuditCheckDetails (Maybe Text)
acdMessage = lens _acdMessage (\s a -> s {_acdMessage = a})

-- | The completion status of this check. One of "IN_PROGRESS", "WAITING_FOR_DATA_COLLECTION", "CANCELED", "COMPLETED_COMPLIANT", "COMPLETED_NON_COMPLIANT", or "FAILED".
acdCheckRunStatus :: Lens' AuditCheckDetails (Maybe AuditCheckRunStatus)
acdCheckRunStatus = lens _acdCheckRunStatus (\s a -> s {_acdCheckRunStatus = a})

instance FromJSON AuditCheckDetails where
  parseJSON =
    withObject
      "AuditCheckDetails"
      ( \x ->
          AuditCheckDetails'
            <$> (x .:? "suppressedNonCompliantResourcesCount")
            <*> (x .:? "totalResourcesCount")
            <*> (x .:? "checkCompliant")
            <*> (x .:? "nonCompliantResourcesCount")
            <*> (x .:? "errorCode")
            <*> (x .:? "message")
            <*> (x .:? "checkRunStatus")
      )

instance Hashable AuditCheckDetails

instance NFData AuditCheckDetails
