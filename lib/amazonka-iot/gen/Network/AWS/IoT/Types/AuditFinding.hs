{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditFinding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditFinding where

import Network.AWS.IoT.Types.AuditFindingSeverity
import Network.AWS.IoT.Types.NonCompliantResource
import Network.AWS.IoT.Types.RelatedResource
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The findings (results) of the audit.
--
--
--
-- /See:/ 'auditFinding' smart constructor.
data AuditFinding = AuditFinding'
  { _afIsSuppressed :: !(Maybe Bool),
    _afTaskId :: !(Maybe Text),
    _afFindingTime :: !(Maybe POSIX),
    _afTaskStartTime :: !(Maybe POSIX),
    _afReasonForNonComplianceCode :: !(Maybe Text),
    _afSeverity :: !(Maybe AuditFindingSeverity),
    _afRelatedResources :: !(Maybe [RelatedResource]),
    _afCheckName :: !(Maybe Text),
    _afNonCompliantResource :: !(Maybe NonCompliantResource),
    _afReasonForNonCompliance :: !(Maybe Text),
    _afFindingId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AuditFinding' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afIsSuppressed' - Indicates whether the audit finding was suppressed or not during reporting.
--
-- * 'afTaskId' - The ID of the audit that generated this result (finding).
--
-- * 'afFindingTime' - The time the result (finding) was discovered.
--
-- * 'afTaskStartTime' - The time the audit started.
--
-- * 'afReasonForNonComplianceCode' - A code that indicates the reason that the resource was noncompliant.
--
-- * 'afSeverity' - The severity of the result (finding).
--
-- * 'afRelatedResources' - The list of related resources.
--
-- * 'afCheckName' - The audit check that generated this result.
--
-- * 'afNonCompliantResource' - The resource that was found to be noncompliant with the audit check.
--
-- * 'afReasonForNonCompliance' - The reason the resource was noncompliant.
--
-- * 'afFindingId' - A unique identifier for this set of audit findings. This identifier is used to apply mitigation tasks to one or more sets of findings.
auditFinding ::
  AuditFinding
auditFinding =
  AuditFinding'
    { _afIsSuppressed = Nothing,
      _afTaskId = Nothing,
      _afFindingTime = Nothing,
      _afTaskStartTime = Nothing,
      _afReasonForNonComplianceCode = Nothing,
      _afSeverity = Nothing,
      _afRelatedResources = Nothing,
      _afCheckName = Nothing,
      _afNonCompliantResource = Nothing,
      _afReasonForNonCompliance = Nothing,
      _afFindingId = Nothing
    }

-- | Indicates whether the audit finding was suppressed or not during reporting.
afIsSuppressed :: Lens' AuditFinding (Maybe Bool)
afIsSuppressed = lens _afIsSuppressed (\s a -> s {_afIsSuppressed = a})

-- | The ID of the audit that generated this result (finding).
afTaskId :: Lens' AuditFinding (Maybe Text)
afTaskId = lens _afTaskId (\s a -> s {_afTaskId = a})

-- | The time the result (finding) was discovered.
afFindingTime :: Lens' AuditFinding (Maybe UTCTime)
afFindingTime = lens _afFindingTime (\s a -> s {_afFindingTime = a}) . mapping _Time

-- | The time the audit started.
afTaskStartTime :: Lens' AuditFinding (Maybe UTCTime)
afTaskStartTime = lens _afTaskStartTime (\s a -> s {_afTaskStartTime = a}) . mapping _Time

-- | A code that indicates the reason that the resource was noncompliant.
afReasonForNonComplianceCode :: Lens' AuditFinding (Maybe Text)
afReasonForNonComplianceCode = lens _afReasonForNonComplianceCode (\s a -> s {_afReasonForNonComplianceCode = a})

-- | The severity of the result (finding).
afSeverity :: Lens' AuditFinding (Maybe AuditFindingSeverity)
afSeverity = lens _afSeverity (\s a -> s {_afSeverity = a})

-- | The list of related resources.
afRelatedResources :: Lens' AuditFinding [RelatedResource]
afRelatedResources = lens _afRelatedResources (\s a -> s {_afRelatedResources = a}) . _Default . _Coerce

-- | The audit check that generated this result.
afCheckName :: Lens' AuditFinding (Maybe Text)
afCheckName = lens _afCheckName (\s a -> s {_afCheckName = a})

-- | The resource that was found to be noncompliant with the audit check.
afNonCompliantResource :: Lens' AuditFinding (Maybe NonCompliantResource)
afNonCompliantResource = lens _afNonCompliantResource (\s a -> s {_afNonCompliantResource = a})

-- | The reason the resource was noncompliant.
afReasonForNonCompliance :: Lens' AuditFinding (Maybe Text)
afReasonForNonCompliance = lens _afReasonForNonCompliance (\s a -> s {_afReasonForNonCompliance = a})

-- | A unique identifier for this set of audit findings. This identifier is used to apply mitigation tasks to one or more sets of findings.
afFindingId :: Lens' AuditFinding (Maybe Text)
afFindingId = lens _afFindingId (\s a -> s {_afFindingId = a})

instance FromJSON AuditFinding where
  parseJSON =
    withObject
      "AuditFinding"
      ( \x ->
          AuditFinding'
            <$> (x .:? "isSuppressed")
            <*> (x .:? "taskId")
            <*> (x .:? "findingTime")
            <*> (x .:? "taskStartTime")
            <*> (x .:? "reasonForNonComplianceCode")
            <*> (x .:? "severity")
            <*> (x .:? "relatedResources" .!= mempty)
            <*> (x .:? "checkName")
            <*> (x .:? "nonCompliantResource")
            <*> (x .:? "reasonForNonCompliance")
            <*> (x .:? "findingId")
      )

instance Hashable AuditFinding

instance NFData AuditFinding
