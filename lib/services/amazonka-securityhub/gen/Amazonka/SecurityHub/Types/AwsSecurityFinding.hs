{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityHub.Types.AwsSecurityFinding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsSecurityFinding where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.Action
import Amazonka.SecurityHub.Types.Compliance
import Amazonka.SecurityHub.Types.FindingProviderFields
import Amazonka.SecurityHub.Types.Malware
import Amazonka.SecurityHub.Types.Network
import Amazonka.SecurityHub.Types.NetworkPathComponent
import Amazonka.SecurityHub.Types.Note
import Amazonka.SecurityHub.Types.PatchSummary
import Amazonka.SecurityHub.Types.ProcessDetails
import Amazonka.SecurityHub.Types.RecordState
import Amazonka.SecurityHub.Types.RelatedFinding
import Amazonka.SecurityHub.Types.Remediation
import Amazonka.SecurityHub.Types.Resource
import Amazonka.SecurityHub.Types.Severity
import Amazonka.SecurityHub.Types.ThreatIntelIndicator
import Amazonka.SecurityHub.Types.VerificationState
import Amazonka.SecurityHub.Types.Vulnerability
import Amazonka.SecurityHub.Types.Workflow
import Amazonka.SecurityHub.Types.WorkflowState

-- | Provides consistent format for the contents of the Security
-- Hub-aggregated findings. @AwsSecurityFinding@ format enables you to
-- share findings between Amazon Web Services security services and
-- third-party solutions, and security standards checks.
--
-- A finding is a potential security issue generated either by Amazon Web
-- Services services or by the integrated third-party solutions and
-- standards checks.
--
-- /See:/ 'newAwsSecurityFinding' smart constructor.
data AwsSecurityFinding = AwsSecurityFinding'
  { -- | The name of the product that generated the finding.
    --
    -- Security Hub populates this attribute automatically for each finding.
    -- You cannot update it using @BatchImportFindings@ or
    -- @BatchUpdateFindings@. The exception to this is when you use a custom
    -- integration.
    --
    -- When you use the Security Hub console to filter findings by product
    -- name, you use this attribute.
    --
    -- When you use the Security Hub API to filter findings by product name,
    -- you use the @aws\/securityhub\/ProductName@ attribute under
    -- @ProductFields@.
    --
    -- Security Hub does not synchronize those two attributes.
    productName :: Prelude.Maybe Prelude.Text,
    -- | The workflow state of a finding.
    workflowState :: Prelude.Maybe WorkflowState,
    -- | The level of importance assigned to the resources associated with the
    -- finding.
    --
    -- A score of 0 means that the underlying resources have no criticality,
    -- and a score of 100 is reserved for the most critical resources.
    criticality :: Prelude.Maybe Prelude.Int,
    -- | The record state of a finding.
    recordState :: Prelude.Maybe RecordState,
    -- | A data type where security-findings providers can include additional
    -- solution-specific details that aren\'t part of the defined
    -- @AwsSecurityFinding@ format.
    --
    -- Can contain up to 50 key-value pairs. For each key-value pair, the key
    -- can contain up to 128 characters, and the value can contain up to 2048
    -- characters.
    productFields :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | This data type is exclusive to findings that are generated as the result
    -- of a check run against a specific rule in a supported security standard,
    -- such as CIS Amazon Web Services Foundations. Contains security
    -- standard-related finding details.
    compliance :: Prelude.Maybe Compliance,
    -- | In a @BatchImportFindings@ request, finding providers use
    -- @FindingProviderFields@ to provide and update their own values for
    -- confidence, criticality, related findings, severity, and types.
    findingProviderFields :: Prelude.Maybe FindingProviderFields,
    -- | The name of the company for the product that generated the finding.
    --
    -- Security Hub populates this attribute automatically for each finding.
    -- You cannot be updated using @BatchImportFindings@ or
    -- @BatchUpdateFindings@. The exception to this is when you use a custom
    -- integration.
    --
    -- When you use the Security Hub console to filter findings by company
    -- name, you use this attribute.
    --
    -- When you use the Security Hub API to filter findings by company name,
    -- you use the @aws\/securityhub\/CompanyName@ attribute under
    -- @ProductFields@.
    --
    -- Security Hub does not synchronize those two attributes.
    companyName :: Prelude.Maybe Prelude.Text,
    -- | A user-defined note added to a finding.
    note :: Prelude.Maybe Note,
    -- | The details of process-related information about a finding.
    process :: Prelude.Maybe ProcessDetails,
    -- | A finding\'s severity.
    severity :: Prelude.Maybe Severity,
    -- | One or more finding types in the format of
    -- @namespace\/category\/classifier@ that classify a finding.
    --
    -- Valid namespace values are: Software and Configuration Checks | TTPs |
    -- Effects | Unusual Behaviors | Sensitive Data Identifications
    types :: Prelude.Maybe [Prelude.Text],
    -- | Provides details about an action that affects or that was taken on a
    -- resource.
    action :: Prelude.Maybe Action,
    -- | The details of network-related information about a finding.
    network :: Prelude.Maybe Network,
    -- | A list of related findings.
    relatedFindings :: Prelude.Maybe [RelatedFinding],
    -- | Indicates when the security-findings provider first observed the
    -- potential security issue that a finding captured.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    firstObservedAt :: Prelude.Maybe Prelude.Text,
    -- | A list of malware related to a finding.
    malware :: Prelude.Maybe [Malware],
    -- | A finding\'s confidence. Confidence is defined as the likelihood that a
    -- finding accurately identifies the behavior or issue that it was intended
    -- to identify.
    --
    -- Confidence is scored on a 0-100 basis using a ratio scale, where 0 means
    -- zero percent confidence and 100 means 100 percent confidence.
    confidence :: Prelude.Maybe Prelude.Int,
    -- | A data type that describes the remediation options for a finding.
    remediation :: Prelude.Maybe Remediation,
    -- | Provides an overview of the patch compliance status for an instance
    -- against a selected compliance standard.
    patchSummary :: Prelude.Maybe PatchSummary,
    -- | Provides a list of vulnerabilities associated with the findings.
    vulnerabilities :: Prelude.Maybe [Vulnerability],
    -- | The Region from which the finding was generated.
    --
    -- Security Hub populates this attribute automatically for each finding.
    -- You cannot update it using @BatchImportFindings@ or
    -- @BatchUpdateFindings@.
    region :: Prelude.Maybe Prelude.Text,
    -- | Provides information about a network path that is relevant to a finding.
    -- Each entry under @NetworkPath@ represents a component of that path.
    networkPath :: Prelude.Maybe [NetworkPathComponent],
    -- | Provides information about the status of the investigation into a
    -- finding.
    workflow :: Prelude.Maybe Workflow,
    -- | Indicates the veracity of a finding.
    verificationState :: Prelude.Maybe VerificationState,
    -- | Threat intelligence details related to a finding.
    threatIntelIndicators :: Prelude.Maybe [ThreatIntelIndicator],
    -- | A URL that links to a page about the current finding in the
    -- security-findings provider\'s solution.
    sourceUrl :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the security-findings provider most recently observed the
    -- potential security issue that a finding captured.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastObservedAt :: Prelude.Maybe Prelude.Text,
    -- | A list of name\/value string pairs associated with the finding. These
    -- are custom, user-defined fields added to a finding.
    userDefinedFields :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The schema version that a finding is formatted for.
    schemaVersion :: Prelude.Text,
    -- | The security findings provider-specific identifier for a finding.
    id :: Prelude.Text,
    -- | The ARN generated by Security Hub that uniquely identifies a product
    -- that generates findings. This can be the ARN for a third-party product
    -- that is integrated with Security Hub, or the ARN for a custom
    -- integration.
    productArn :: Prelude.Text,
    -- | The identifier for the solution-specific component (a discrete unit of
    -- logic) that generated a finding. In various security-findings
    -- providers\' solutions, this generator can be called a rule, a check, a
    -- detector, a plugin, etc.
    generatorId :: Prelude.Text,
    -- | The Amazon Web Services account ID that a finding is generated in.
    awsAccountId :: Prelude.Text,
    -- | Indicates when the security-findings provider created the potential
    -- security issue that a finding captured.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdAt :: Prelude.Text,
    -- | Indicates when the security-findings provider last updated the finding
    -- record.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    updatedAt :: Prelude.Text,
    -- | A finding\'s title.
    --
    -- In this release, @Title@ is a required property.
    title :: Prelude.Text,
    -- | A finding\'s description.
    --
    -- In this release, @Description@ is a required property.
    description :: Prelude.Text,
    -- | A set of resource data types that describe the resources that the
    -- finding refers to.
    resources :: [Resource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsSecurityFinding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productName', 'awsSecurityFinding_productName' - The name of the product that generated the finding.
--
-- Security Hub populates this attribute automatically for each finding.
-- You cannot update it using @BatchImportFindings@ or
-- @BatchUpdateFindings@. The exception to this is when you use a custom
-- integration.
--
-- When you use the Security Hub console to filter findings by product
-- name, you use this attribute.
--
-- When you use the Security Hub API to filter findings by product name,
-- you use the @aws\/securityhub\/ProductName@ attribute under
-- @ProductFields@.
--
-- Security Hub does not synchronize those two attributes.
--
-- 'workflowState', 'awsSecurityFinding_workflowState' - The workflow state of a finding.
--
-- 'criticality', 'awsSecurityFinding_criticality' - The level of importance assigned to the resources associated with the
-- finding.
--
-- A score of 0 means that the underlying resources have no criticality,
-- and a score of 100 is reserved for the most critical resources.
--
-- 'recordState', 'awsSecurityFinding_recordState' - The record state of a finding.
--
-- 'productFields', 'awsSecurityFinding_productFields' - A data type where security-findings providers can include additional
-- solution-specific details that aren\'t part of the defined
-- @AwsSecurityFinding@ format.
--
-- Can contain up to 50 key-value pairs. For each key-value pair, the key
-- can contain up to 128 characters, and the value can contain up to 2048
-- characters.
--
-- 'compliance', 'awsSecurityFinding_compliance' - This data type is exclusive to findings that are generated as the result
-- of a check run against a specific rule in a supported security standard,
-- such as CIS Amazon Web Services Foundations. Contains security
-- standard-related finding details.
--
-- 'findingProviderFields', 'awsSecurityFinding_findingProviderFields' - In a @BatchImportFindings@ request, finding providers use
-- @FindingProviderFields@ to provide and update their own values for
-- confidence, criticality, related findings, severity, and types.
--
-- 'companyName', 'awsSecurityFinding_companyName' - The name of the company for the product that generated the finding.
--
-- Security Hub populates this attribute automatically for each finding.
-- You cannot be updated using @BatchImportFindings@ or
-- @BatchUpdateFindings@. The exception to this is when you use a custom
-- integration.
--
-- When you use the Security Hub console to filter findings by company
-- name, you use this attribute.
--
-- When you use the Security Hub API to filter findings by company name,
-- you use the @aws\/securityhub\/CompanyName@ attribute under
-- @ProductFields@.
--
-- Security Hub does not synchronize those two attributes.
--
-- 'note', 'awsSecurityFinding_note' - A user-defined note added to a finding.
--
-- 'process', 'awsSecurityFinding_process' - The details of process-related information about a finding.
--
-- 'severity', 'awsSecurityFinding_severity' - A finding\'s severity.
--
-- 'types', 'awsSecurityFinding_types' - One or more finding types in the format of
-- @namespace\/category\/classifier@ that classify a finding.
--
-- Valid namespace values are: Software and Configuration Checks | TTPs |
-- Effects | Unusual Behaviors | Sensitive Data Identifications
--
-- 'action', 'awsSecurityFinding_action' - Provides details about an action that affects or that was taken on a
-- resource.
--
-- 'network', 'awsSecurityFinding_network' - The details of network-related information about a finding.
--
-- 'relatedFindings', 'awsSecurityFinding_relatedFindings' - A list of related findings.
--
-- 'firstObservedAt', 'awsSecurityFinding_firstObservedAt' - Indicates when the security-findings provider first observed the
-- potential security issue that a finding captured.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'malware', 'awsSecurityFinding_malware' - A list of malware related to a finding.
--
-- 'confidence', 'awsSecurityFinding_confidence' - A finding\'s confidence. Confidence is defined as the likelihood that a
-- finding accurately identifies the behavior or issue that it was intended
-- to identify.
--
-- Confidence is scored on a 0-100 basis using a ratio scale, where 0 means
-- zero percent confidence and 100 means 100 percent confidence.
--
-- 'remediation', 'awsSecurityFinding_remediation' - A data type that describes the remediation options for a finding.
--
-- 'patchSummary', 'awsSecurityFinding_patchSummary' - Provides an overview of the patch compliance status for an instance
-- against a selected compliance standard.
--
-- 'vulnerabilities', 'awsSecurityFinding_vulnerabilities' - Provides a list of vulnerabilities associated with the findings.
--
-- 'region', 'awsSecurityFinding_region' - The Region from which the finding was generated.
--
-- Security Hub populates this attribute automatically for each finding.
-- You cannot update it using @BatchImportFindings@ or
-- @BatchUpdateFindings@.
--
-- 'networkPath', 'awsSecurityFinding_networkPath' - Provides information about a network path that is relevant to a finding.
-- Each entry under @NetworkPath@ represents a component of that path.
--
-- 'workflow', 'awsSecurityFinding_workflow' - Provides information about the status of the investigation into a
-- finding.
--
-- 'verificationState', 'awsSecurityFinding_verificationState' - Indicates the veracity of a finding.
--
-- 'threatIntelIndicators', 'awsSecurityFinding_threatIntelIndicators' - Threat intelligence details related to a finding.
--
-- 'sourceUrl', 'awsSecurityFinding_sourceUrl' - A URL that links to a page about the current finding in the
-- security-findings provider\'s solution.
--
-- 'lastObservedAt', 'awsSecurityFinding_lastObservedAt' - Indicates when the security-findings provider most recently observed the
-- potential security issue that a finding captured.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'userDefinedFields', 'awsSecurityFinding_userDefinedFields' - A list of name\/value string pairs associated with the finding. These
-- are custom, user-defined fields added to a finding.
--
-- 'schemaVersion', 'awsSecurityFinding_schemaVersion' - The schema version that a finding is formatted for.
--
-- 'id', 'awsSecurityFinding_id' - The security findings provider-specific identifier for a finding.
--
-- 'productArn', 'awsSecurityFinding_productArn' - The ARN generated by Security Hub that uniquely identifies a product
-- that generates findings. This can be the ARN for a third-party product
-- that is integrated with Security Hub, or the ARN for a custom
-- integration.
--
-- 'generatorId', 'awsSecurityFinding_generatorId' - The identifier for the solution-specific component (a discrete unit of
-- logic) that generated a finding. In various security-findings
-- providers\' solutions, this generator can be called a rule, a check, a
-- detector, a plugin, etc.
--
-- 'awsAccountId', 'awsSecurityFinding_awsAccountId' - The Amazon Web Services account ID that a finding is generated in.
--
-- 'createdAt', 'awsSecurityFinding_createdAt' - Indicates when the security-findings provider created the potential
-- security issue that a finding captured.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'updatedAt', 'awsSecurityFinding_updatedAt' - Indicates when the security-findings provider last updated the finding
-- record.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'title', 'awsSecurityFinding_title' - A finding\'s title.
--
-- In this release, @Title@ is a required property.
--
-- 'description', 'awsSecurityFinding_description' - A finding\'s description.
--
-- In this release, @Description@ is a required property.
--
-- 'resources', 'awsSecurityFinding_resources' - A set of resource data types that describe the resources that the
-- finding refers to.
newAwsSecurityFinding ::
  -- | 'schemaVersion'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'productArn'
  Prelude.Text ->
  -- | 'generatorId'
  Prelude.Text ->
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.Text ->
  -- | 'updatedAt'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  AwsSecurityFinding
newAwsSecurityFinding
  pSchemaVersion_
  pId_
  pProductArn_
  pGeneratorId_
  pAwsAccountId_
  pCreatedAt_
  pUpdatedAt_
  pTitle_
  pDescription_ =
    AwsSecurityFinding'
      { productName = Prelude.Nothing,
        workflowState = Prelude.Nothing,
        criticality = Prelude.Nothing,
        recordState = Prelude.Nothing,
        productFields = Prelude.Nothing,
        compliance = Prelude.Nothing,
        findingProviderFields = Prelude.Nothing,
        companyName = Prelude.Nothing,
        note = Prelude.Nothing,
        process = Prelude.Nothing,
        severity = Prelude.Nothing,
        types = Prelude.Nothing,
        action = Prelude.Nothing,
        network = Prelude.Nothing,
        relatedFindings = Prelude.Nothing,
        firstObservedAt = Prelude.Nothing,
        malware = Prelude.Nothing,
        confidence = Prelude.Nothing,
        remediation = Prelude.Nothing,
        patchSummary = Prelude.Nothing,
        vulnerabilities = Prelude.Nothing,
        region = Prelude.Nothing,
        networkPath = Prelude.Nothing,
        workflow = Prelude.Nothing,
        verificationState = Prelude.Nothing,
        threatIntelIndicators = Prelude.Nothing,
        sourceUrl = Prelude.Nothing,
        lastObservedAt = Prelude.Nothing,
        userDefinedFields = Prelude.Nothing,
        schemaVersion = pSchemaVersion_,
        id = pId_,
        productArn = pProductArn_,
        generatorId = pGeneratorId_,
        awsAccountId = pAwsAccountId_,
        createdAt = pCreatedAt_,
        updatedAt = pUpdatedAt_,
        title = pTitle_,
        description = pDescription_,
        resources = Prelude.mempty
      }

-- | The name of the product that generated the finding.
--
-- Security Hub populates this attribute automatically for each finding.
-- You cannot update it using @BatchImportFindings@ or
-- @BatchUpdateFindings@. The exception to this is when you use a custom
-- integration.
--
-- When you use the Security Hub console to filter findings by product
-- name, you use this attribute.
--
-- When you use the Security Hub API to filter findings by product name,
-- you use the @aws\/securityhub\/ProductName@ attribute under
-- @ProductFields@.
--
-- Security Hub does not synchronize those two attributes.
awsSecurityFinding_productName :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe Prelude.Text)
awsSecurityFinding_productName = Lens.lens (\AwsSecurityFinding' {productName} -> productName) (\s@AwsSecurityFinding' {} a -> s {productName = a} :: AwsSecurityFinding)

-- | The workflow state of a finding.
awsSecurityFinding_workflowState :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe WorkflowState)
awsSecurityFinding_workflowState = Lens.lens (\AwsSecurityFinding' {workflowState} -> workflowState) (\s@AwsSecurityFinding' {} a -> s {workflowState = a} :: AwsSecurityFinding)

-- | The level of importance assigned to the resources associated with the
-- finding.
--
-- A score of 0 means that the underlying resources have no criticality,
-- and a score of 100 is reserved for the most critical resources.
awsSecurityFinding_criticality :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe Prelude.Int)
awsSecurityFinding_criticality = Lens.lens (\AwsSecurityFinding' {criticality} -> criticality) (\s@AwsSecurityFinding' {} a -> s {criticality = a} :: AwsSecurityFinding)

-- | The record state of a finding.
awsSecurityFinding_recordState :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe RecordState)
awsSecurityFinding_recordState = Lens.lens (\AwsSecurityFinding' {recordState} -> recordState) (\s@AwsSecurityFinding' {} a -> s {recordState = a} :: AwsSecurityFinding)

-- | A data type where security-findings providers can include additional
-- solution-specific details that aren\'t part of the defined
-- @AwsSecurityFinding@ format.
--
-- Can contain up to 50 key-value pairs. For each key-value pair, the key
-- can contain up to 128 characters, and the value can contain up to 2048
-- characters.
awsSecurityFinding_productFields :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
awsSecurityFinding_productFields = Lens.lens (\AwsSecurityFinding' {productFields} -> productFields) (\s@AwsSecurityFinding' {} a -> s {productFields = a} :: AwsSecurityFinding) Prelude.. Lens.mapping Lens.coerced

-- | This data type is exclusive to findings that are generated as the result
-- of a check run against a specific rule in a supported security standard,
-- such as CIS Amazon Web Services Foundations. Contains security
-- standard-related finding details.
awsSecurityFinding_compliance :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe Compliance)
awsSecurityFinding_compliance = Lens.lens (\AwsSecurityFinding' {compliance} -> compliance) (\s@AwsSecurityFinding' {} a -> s {compliance = a} :: AwsSecurityFinding)

-- | In a @BatchImportFindings@ request, finding providers use
-- @FindingProviderFields@ to provide and update their own values for
-- confidence, criticality, related findings, severity, and types.
awsSecurityFinding_findingProviderFields :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe FindingProviderFields)
awsSecurityFinding_findingProviderFields = Lens.lens (\AwsSecurityFinding' {findingProviderFields} -> findingProviderFields) (\s@AwsSecurityFinding' {} a -> s {findingProviderFields = a} :: AwsSecurityFinding)

-- | The name of the company for the product that generated the finding.
--
-- Security Hub populates this attribute automatically for each finding.
-- You cannot be updated using @BatchImportFindings@ or
-- @BatchUpdateFindings@. The exception to this is when you use a custom
-- integration.
--
-- When you use the Security Hub console to filter findings by company
-- name, you use this attribute.
--
-- When you use the Security Hub API to filter findings by company name,
-- you use the @aws\/securityhub\/CompanyName@ attribute under
-- @ProductFields@.
--
-- Security Hub does not synchronize those two attributes.
awsSecurityFinding_companyName :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe Prelude.Text)
awsSecurityFinding_companyName = Lens.lens (\AwsSecurityFinding' {companyName} -> companyName) (\s@AwsSecurityFinding' {} a -> s {companyName = a} :: AwsSecurityFinding)

-- | A user-defined note added to a finding.
awsSecurityFinding_note :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe Note)
awsSecurityFinding_note = Lens.lens (\AwsSecurityFinding' {note} -> note) (\s@AwsSecurityFinding' {} a -> s {note = a} :: AwsSecurityFinding)

-- | The details of process-related information about a finding.
awsSecurityFinding_process :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe ProcessDetails)
awsSecurityFinding_process = Lens.lens (\AwsSecurityFinding' {process} -> process) (\s@AwsSecurityFinding' {} a -> s {process = a} :: AwsSecurityFinding)

-- | A finding\'s severity.
awsSecurityFinding_severity :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe Severity)
awsSecurityFinding_severity = Lens.lens (\AwsSecurityFinding' {severity} -> severity) (\s@AwsSecurityFinding' {} a -> s {severity = a} :: AwsSecurityFinding)

-- | One or more finding types in the format of
-- @namespace\/category\/classifier@ that classify a finding.
--
-- Valid namespace values are: Software and Configuration Checks | TTPs |
-- Effects | Unusual Behaviors | Sensitive Data Identifications
awsSecurityFinding_types :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe [Prelude.Text])
awsSecurityFinding_types = Lens.lens (\AwsSecurityFinding' {types} -> types) (\s@AwsSecurityFinding' {} a -> s {types = a} :: AwsSecurityFinding) Prelude.. Lens.mapping Lens.coerced

-- | Provides details about an action that affects or that was taken on a
-- resource.
awsSecurityFinding_action :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe Action)
awsSecurityFinding_action = Lens.lens (\AwsSecurityFinding' {action} -> action) (\s@AwsSecurityFinding' {} a -> s {action = a} :: AwsSecurityFinding)

-- | The details of network-related information about a finding.
awsSecurityFinding_network :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe Network)
awsSecurityFinding_network = Lens.lens (\AwsSecurityFinding' {network} -> network) (\s@AwsSecurityFinding' {} a -> s {network = a} :: AwsSecurityFinding)

-- | A list of related findings.
awsSecurityFinding_relatedFindings :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe [RelatedFinding])
awsSecurityFinding_relatedFindings = Lens.lens (\AwsSecurityFinding' {relatedFindings} -> relatedFindings) (\s@AwsSecurityFinding' {} a -> s {relatedFindings = a} :: AwsSecurityFinding) Prelude.. Lens.mapping Lens.coerced

-- | Indicates when the security-findings provider first observed the
-- potential security issue that a finding captured.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsSecurityFinding_firstObservedAt :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe Prelude.Text)
awsSecurityFinding_firstObservedAt = Lens.lens (\AwsSecurityFinding' {firstObservedAt} -> firstObservedAt) (\s@AwsSecurityFinding' {} a -> s {firstObservedAt = a} :: AwsSecurityFinding)

-- | A list of malware related to a finding.
awsSecurityFinding_malware :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe [Malware])
awsSecurityFinding_malware = Lens.lens (\AwsSecurityFinding' {malware} -> malware) (\s@AwsSecurityFinding' {} a -> s {malware = a} :: AwsSecurityFinding) Prelude.. Lens.mapping Lens.coerced

-- | A finding\'s confidence. Confidence is defined as the likelihood that a
-- finding accurately identifies the behavior or issue that it was intended
-- to identify.
--
-- Confidence is scored on a 0-100 basis using a ratio scale, where 0 means
-- zero percent confidence and 100 means 100 percent confidence.
awsSecurityFinding_confidence :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe Prelude.Int)
awsSecurityFinding_confidence = Lens.lens (\AwsSecurityFinding' {confidence} -> confidence) (\s@AwsSecurityFinding' {} a -> s {confidence = a} :: AwsSecurityFinding)

-- | A data type that describes the remediation options for a finding.
awsSecurityFinding_remediation :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe Remediation)
awsSecurityFinding_remediation = Lens.lens (\AwsSecurityFinding' {remediation} -> remediation) (\s@AwsSecurityFinding' {} a -> s {remediation = a} :: AwsSecurityFinding)

-- | Provides an overview of the patch compliance status for an instance
-- against a selected compliance standard.
awsSecurityFinding_patchSummary :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe PatchSummary)
awsSecurityFinding_patchSummary = Lens.lens (\AwsSecurityFinding' {patchSummary} -> patchSummary) (\s@AwsSecurityFinding' {} a -> s {patchSummary = a} :: AwsSecurityFinding)

-- | Provides a list of vulnerabilities associated with the findings.
awsSecurityFinding_vulnerabilities :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe [Vulnerability])
awsSecurityFinding_vulnerabilities = Lens.lens (\AwsSecurityFinding' {vulnerabilities} -> vulnerabilities) (\s@AwsSecurityFinding' {} a -> s {vulnerabilities = a} :: AwsSecurityFinding) Prelude.. Lens.mapping Lens.coerced

-- | The Region from which the finding was generated.
--
-- Security Hub populates this attribute automatically for each finding.
-- You cannot update it using @BatchImportFindings@ or
-- @BatchUpdateFindings@.
awsSecurityFinding_region :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe Prelude.Text)
awsSecurityFinding_region = Lens.lens (\AwsSecurityFinding' {region} -> region) (\s@AwsSecurityFinding' {} a -> s {region = a} :: AwsSecurityFinding)

-- | Provides information about a network path that is relevant to a finding.
-- Each entry under @NetworkPath@ represents a component of that path.
awsSecurityFinding_networkPath :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe [NetworkPathComponent])
awsSecurityFinding_networkPath = Lens.lens (\AwsSecurityFinding' {networkPath} -> networkPath) (\s@AwsSecurityFinding' {} a -> s {networkPath = a} :: AwsSecurityFinding) Prelude.. Lens.mapping Lens.coerced

-- | Provides information about the status of the investigation into a
-- finding.
awsSecurityFinding_workflow :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe Workflow)
awsSecurityFinding_workflow = Lens.lens (\AwsSecurityFinding' {workflow} -> workflow) (\s@AwsSecurityFinding' {} a -> s {workflow = a} :: AwsSecurityFinding)

-- | Indicates the veracity of a finding.
awsSecurityFinding_verificationState :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe VerificationState)
awsSecurityFinding_verificationState = Lens.lens (\AwsSecurityFinding' {verificationState} -> verificationState) (\s@AwsSecurityFinding' {} a -> s {verificationState = a} :: AwsSecurityFinding)

-- | Threat intelligence details related to a finding.
awsSecurityFinding_threatIntelIndicators :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe [ThreatIntelIndicator])
awsSecurityFinding_threatIntelIndicators = Lens.lens (\AwsSecurityFinding' {threatIntelIndicators} -> threatIntelIndicators) (\s@AwsSecurityFinding' {} a -> s {threatIntelIndicators = a} :: AwsSecurityFinding) Prelude.. Lens.mapping Lens.coerced

-- | A URL that links to a page about the current finding in the
-- security-findings provider\'s solution.
awsSecurityFinding_sourceUrl :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe Prelude.Text)
awsSecurityFinding_sourceUrl = Lens.lens (\AwsSecurityFinding' {sourceUrl} -> sourceUrl) (\s@AwsSecurityFinding' {} a -> s {sourceUrl = a} :: AwsSecurityFinding)

-- | Indicates when the security-findings provider most recently observed the
-- potential security issue that a finding captured.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsSecurityFinding_lastObservedAt :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe Prelude.Text)
awsSecurityFinding_lastObservedAt = Lens.lens (\AwsSecurityFinding' {lastObservedAt} -> lastObservedAt) (\s@AwsSecurityFinding' {} a -> s {lastObservedAt = a} :: AwsSecurityFinding)

-- | A list of name\/value string pairs associated with the finding. These
-- are custom, user-defined fields added to a finding.
awsSecurityFinding_userDefinedFields :: Lens.Lens' AwsSecurityFinding (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
awsSecurityFinding_userDefinedFields = Lens.lens (\AwsSecurityFinding' {userDefinedFields} -> userDefinedFields) (\s@AwsSecurityFinding' {} a -> s {userDefinedFields = a} :: AwsSecurityFinding) Prelude.. Lens.mapping Lens.coerced

-- | The schema version that a finding is formatted for.
awsSecurityFinding_schemaVersion :: Lens.Lens' AwsSecurityFinding Prelude.Text
awsSecurityFinding_schemaVersion = Lens.lens (\AwsSecurityFinding' {schemaVersion} -> schemaVersion) (\s@AwsSecurityFinding' {} a -> s {schemaVersion = a} :: AwsSecurityFinding)

-- | The security findings provider-specific identifier for a finding.
awsSecurityFinding_id :: Lens.Lens' AwsSecurityFinding Prelude.Text
awsSecurityFinding_id = Lens.lens (\AwsSecurityFinding' {id} -> id) (\s@AwsSecurityFinding' {} a -> s {id = a} :: AwsSecurityFinding)

-- | The ARN generated by Security Hub that uniquely identifies a product
-- that generates findings. This can be the ARN for a third-party product
-- that is integrated with Security Hub, or the ARN for a custom
-- integration.
awsSecurityFinding_productArn :: Lens.Lens' AwsSecurityFinding Prelude.Text
awsSecurityFinding_productArn = Lens.lens (\AwsSecurityFinding' {productArn} -> productArn) (\s@AwsSecurityFinding' {} a -> s {productArn = a} :: AwsSecurityFinding)

-- | The identifier for the solution-specific component (a discrete unit of
-- logic) that generated a finding. In various security-findings
-- providers\' solutions, this generator can be called a rule, a check, a
-- detector, a plugin, etc.
awsSecurityFinding_generatorId :: Lens.Lens' AwsSecurityFinding Prelude.Text
awsSecurityFinding_generatorId = Lens.lens (\AwsSecurityFinding' {generatorId} -> generatorId) (\s@AwsSecurityFinding' {} a -> s {generatorId = a} :: AwsSecurityFinding)

-- | The Amazon Web Services account ID that a finding is generated in.
awsSecurityFinding_awsAccountId :: Lens.Lens' AwsSecurityFinding Prelude.Text
awsSecurityFinding_awsAccountId = Lens.lens (\AwsSecurityFinding' {awsAccountId} -> awsAccountId) (\s@AwsSecurityFinding' {} a -> s {awsAccountId = a} :: AwsSecurityFinding)

-- | Indicates when the security-findings provider created the potential
-- security issue that a finding captured.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsSecurityFinding_createdAt :: Lens.Lens' AwsSecurityFinding Prelude.Text
awsSecurityFinding_createdAt = Lens.lens (\AwsSecurityFinding' {createdAt} -> createdAt) (\s@AwsSecurityFinding' {} a -> s {createdAt = a} :: AwsSecurityFinding)

-- | Indicates when the security-findings provider last updated the finding
-- record.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsSecurityFinding_updatedAt :: Lens.Lens' AwsSecurityFinding Prelude.Text
awsSecurityFinding_updatedAt = Lens.lens (\AwsSecurityFinding' {updatedAt} -> updatedAt) (\s@AwsSecurityFinding' {} a -> s {updatedAt = a} :: AwsSecurityFinding)

-- | A finding\'s title.
--
-- In this release, @Title@ is a required property.
awsSecurityFinding_title :: Lens.Lens' AwsSecurityFinding Prelude.Text
awsSecurityFinding_title = Lens.lens (\AwsSecurityFinding' {title} -> title) (\s@AwsSecurityFinding' {} a -> s {title = a} :: AwsSecurityFinding)

-- | A finding\'s description.
--
-- In this release, @Description@ is a required property.
awsSecurityFinding_description :: Lens.Lens' AwsSecurityFinding Prelude.Text
awsSecurityFinding_description = Lens.lens (\AwsSecurityFinding' {description} -> description) (\s@AwsSecurityFinding' {} a -> s {description = a} :: AwsSecurityFinding)

-- | A set of resource data types that describe the resources that the
-- finding refers to.
awsSecurityFinding_resources :: Lens.Lens' AwsSecurityFinding [Resource]
awsSecurityFinding_resources = Lens.lens (\AwsSecurityFinding' {resources} -> resources) (\s@AwsSecurityFinding' {} a -> s {resources = a} :: AwsSecurityFinding) Prelude.. Lens.coerced

instance Core.FromJSON AwsSecurityFinding where
  parseJSON =
    Core.withObject
      "AwsSecurityFinding"
      ( \x ->
          AwsSecurityFinding'
            Prelude.<$> (x Core..:? "ProductName")
            Prelude.<*> (x Core..:? "WorkflowState")
            Prelude.<*> (x Core..:? "Criticality")
            Prelude.<*> (x Core..:? "RecordState")
            Prelude.<*> (x Core..:? "ProductFields" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Compliance")
            Prelude.<*> (x Core..:? "FindingProviderFields")
            Prelude.<*> (x Core..:? "CompanyName")
            Prelude.<*> (x Core..:? "Note")
            Prelude.<*> (x Core..:? "Process")
            Prelude.<*> (x Core..:? "Severity")
            Prelude.<*> (x Core..:? "Types" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Action")
            Prelude.<*> (x Core..:? "Network")
            Prelude.<*> ( x Core..:? "RelatedFindings"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "FirstObservedAt")
            Prelude.<*> (x Core..:? "Malware" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Confidence")
            Prelude.<*> (x Core..:? "Remediation")
            Prelude.<*> (x Core..:? "PatchSummary")
            Prelude.<*> ( x Core..:? "Vulnerabilities"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Region")
            Prelude.<*> (x Core..:? "NetworkPath" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Workflow")
            Prelude.<*> (x Core..:? "VerificationState")
            Prelude.<*> ( x Core..:? "ThreatIntelIndicators"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "SourceUrl")
            Prelude.<*> (x Core..:? "LastObservedAt")
            Prelude.<*> ( x Core..:? "UserDefinedFields"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "SchemaVersion")
            Prelude.<*> (x Core..: "Id")
            Prelude.<*> (x Core..: "ProductArn")
            Prelude.<*> (x Core..: "GeneratorId")
            Prelude.<*> (x Core..: "AwsAccountId")
            Prelude.<*> (x Core..: "CreatedAt")
            Prelude.<*> (x Core..: "UpdatedAt")
            Prelude.<*> (x Core..: "Title")
            Prelude.<*> (x Core..: "Description")
            Prelude.<*> (x Core..:? "Resources" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable AwsSecurityFinding

instance Prelude.NFData AwsSecurityFinding

instance Core.ToJSON AwsSecurityFinding where
  toJSON AwsSecurityFinding' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ProductName" Core..=) Prelude.<$> productName,
            ("WorkflowState" Core..=) Prelude.<$> workflowState,
            ("Criticality" Core..=) Prelude.<$> criticality,
            ("RecordState" Core..=) Prelude.<$> recordState,
            ("ProductFields" Core..=) Prelude.<$> productFields,
            ("Compliance" Core..=) Prelude.<$> compliance,
            ("FindingProviderFields" Core..=)
              Prelude.<$> findingProviderFields,
            ("CompanyName" Core..=) Prelude.<$> companyName,
            ("Note" Core..=) Prelude.<$> note,
            ("Process" Core..=) Prelude.<$> process,
            ("Severity" Core..=) Prelude.<$> severity,
            ("Types" Core..=) Prelude.<$> types,
            ("Action" Core..=) Prelude.<$> action,
            ("Network" Core..=) Prelude.<$> network,
            ("RelatedFindings" Core..=)
              Prelude.<$> relatedFindings,
            ("FirstObservedAt" Core..=)
              Prelude.<$> firstObservedAt,
            ("Malware" Core..=) Prelude.<$> malware,
            ("Confidence" Core..=) Prelude.<$> confidence,
            ("Remediation" Core..=) Prelude.<$> remediation,
            ("PatchSummary" Core..=) Prelude.<$> patchSummary,
            ("Vulnerabilities" Core..=)
              Prelude.<$> vulnerabilities,
            ("Region" Core..=) Prelude.<$> region,
            ("NetworkPath" Core..=) Prelude.<$> networkPath,
            ("Workflow" Core..=) Prelude.<$> workflow,
            ("VerificationState" Core..=)
              Prelude.<$> verificationState,
            ("ThreatIntelIndicators" Core..=)
              Prelude.<$> threatIntelIndicators,
            ("SourceUrl" Core..=) Prelude.<$> sourceUrl,
            ("LastObservedAt" Core..=)
              Prelude.<$> lastObservedAt,
            ("UserDefinedFields" Core..=)
              Prelude.<$> userDefinedFields,
            Prelude.Just ("SchemaVersion" Core..= schemaVersion),
            Prelude.Just ("Id" Core..= id),
            Prelude.Just ("ProductArn" Core..= productArn),
            Prelude.Just ("GeneratorId" Core..= generatorId),
            Prelude.Just ("AwsAccountId" Core..= awsAccountId),
            Prelude.Just ("CreatedAt" Core..= createdAt),
            Prelude.Just ("UpdatedAt" Core..= updatedAt),
            Prelude.Just ("Title" Core..= title),
            Prelude.Just ("Description" Core..= description),
            Prelude.Just ("Resources" Core..= resources)
          ]
      )
