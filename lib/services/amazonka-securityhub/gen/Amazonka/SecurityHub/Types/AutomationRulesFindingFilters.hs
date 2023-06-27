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
-- Module      : Amazonka.SecurityHub.Types.AutomationRulesFindingFilters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AutomationRulesFindingFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.DateFilter
import Amazonka.SecurityHub.Types.MapFilter
import Amazonka.SecurityHub.Types.NumberFilter
import Amazonka.SecurityHub.Types.StringFilter

-- | The criteria that determine which findings a rule applies to.
--
-- /See:/ 'newAutomationRulesFindingFilters' smart constructor.
data AutomationRulesFindingFilters = AutomationRulesFindingFilters'
  { -- | The Amazon Web Services account ID in which a finding was generated.
    awsAccountId :: Prelude.Maybe [StringFilter],
    -- | The name of the company for the product that generated the finding. For
    -- control-based findings, the company is Amazon Web Services.
    companyName :: Prelude.Maybe [StringFilter],
    -- | The unique identifier of a standard in which a control is enabled. This
    -- field consists of the resource portion of the Amazon Resource Name (ARN)
    -- returned for a standard in the
    -- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_DescribeStandards.html DescribeStandards>
    -- API response.
    complianceAssociatedStandardsId :: Prelude.Maybe [StringFilter],
    -- | The security control ID for which a finding was generated. Security
    -- control IDs are the same across standards.
    complianceSecurityControlId :: Prelude.Maybe [StringFilter],
    -- | The result of a security check. This field is only used for findings
    -- generated from controls.
    complianceStatus :: Prelude.Maybe [StringFilter],
    -- | The likelihood that a finding accurately identifies the behavior or
    -- issue that it was intended to identify. @Confidence@ is scored on a
    -- 0–100 basis using a ratio scale. A value of @0@ means 0 percent
    -- confidence, and a value of @100@ means 100 percent confidence. For
    -- example, a data exfiltration detection based on a statistical deviation
    -- of network traffic has low confidence because an actual exfiltration
    -- hasn\'t been verified. For more information, see
    -- <https://docs.aws.amazon.com/securityhub/latest/userguide/asff-top-level-attributes.html#asff-confidence Confidence>
    -- in the /Security Hub User Guide/.
    confidence :: Prelude.Maybe [NumberFilter],
    -- | A timestamp that indicates when this finding record was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdAt :: Prelude.Maybe [DateFilter],
    -- | The level of importance that is assigned to the resources that are
    -- associated with a finding. @Criticality@ is scored on a 0–100 basis,
    -- using a ratio scale that supports only full integers. A score of @0@
    -- means that the underlying resources have no criticality, and a score of
    -- @100@ is reserved for the most critical resources. For more information,
    -- see
    -- <https://docs.aws.amazon.com/securityhub/latest/userguide/asff-top-level-attributes.html#asff-criticality Criticality>
    -- in the /Security Hub User Guide/.
    criticality :: Prelude.Maybe [NumberFilter],
    -- | A finding\'s description.
    description :: Prelude.Maybe [StringFilter],
    -- | A timestamp that indicates when the potential security issue captured by
    -- a finding was first observed by the security findings product.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    firstObservedAt :: Prelude.Maybe [DateFilter],
    -- | The identifier for the solution-specific component that generated a
    -- finding.
    generatorId :: Prelude.Maybe [StringFilter],
    -- | The product-specific identifier for a finding.
    id :: Prelude.Maybe [StringFilter],
    -- | A timestamp that indicates when the potential security issue captured by
    -- a finding was most recently observed by the security findings product.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    lastObservedAt :: Prelude.Maybe [DateFilter],
    -- | The text of a user-defined note that\'s added to a finding.
    noteText :: Prelude.Maybe [StringFilter],
    -- | The timestamp of when the note was updated. Uses the date-time format
    -- specified in
    -- <https://www.rfc-editor.org/rfc/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    noteUpdatedAt :: Prelude.Maybe [DateFilter],
    -- | The principal that created a note.
    noteUpdatedBy :: Prelude.Maybe [StringFilter],
    -- | The Amazon Resource Name (ARN) for a third-party product that generated
    -- a finding in Security Hub.
    productArn :: Prelude.Maybe [StringFilter],
    -- | Provides the name of the product that generated the finding. For
    -- control-based findings, the product name is Security Hub.
    productName :: Prelude.Maybe [StringFilter],
    -- | Provides the current state of a finding.
    recordState :: Prelude.Maybe [StringFilter],
    -- | The product-generated identifier for a related finding.
    relatedFindingsId :: Prelude.Maybe [StringFilter],
    -- | The ARN for the product that generated a related finding.
    relatedFindingsProductArn :: Prelude.Maybe [StringFilter],
    -- | Custom fields and values about the resource that a finding pertains to.
    resourceDetailsOther :: Prelude.Maybe [MapFilter],
    -- | The identifier for the given resource type. For Amazon Web Services
    -- resources that are identified by Amazon Resource Names (ARNs), this is
    -- the ARN. For Amazon Web Services resources that lack ARNs, this is the
    -- identifier as defined by the Amazon Web Service that created the
    -- resource. For non-Amazon Web Services resources, this is a unique
    -- identifier that is associated with the resource.
    resourceId :: Prelude.Maybe [StringFilter],
    -- | The partition in which the resource that the finding pertains to is
    -- located. A partition is a group of Amazon Web Services Regions. Each
    -- Amazon Web Services account is scoped to one partition.
    resourcePartition :: Prelude.Maybe [StringFilter],
    -- | The Amazon Web Services Region where the resource that a finding
    -- pertains to is located.
    resourceRegion :: Prelude.Maybe [StringFilter],
    -- | A list of Amazon Web Services tags associated with a resource at the
    -- time the finding was processed.
    resourceTags :: Prelude.Maybe [MapFilter],
    -- | The type of resource that the finding pertains to.
    resourceType :: Prelude.Maybe [StringFilter],
    -- | The severity value of the finding.
    severityLabel :: Prelude.Maybe [StringFilter],
    -- | Provides a URL that links to a page about the current finding in the
    -- finding product.
    sourceUrl :: Prelude.Maybe [StringFilter],
    -- | A finding\'s title.
    title :: Prelude.Maybe [StringFilter],
    -- | One or more finding types in the format of
    -- namespace\/category\/classifier that classify a finding. For a list of
    -- namespaces, classifiers, and categories, see
    -- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-findings-format-type-taxonomy.html Types taxonomy for ASFF>
    -- in the /Security Hub User Guide/.
    type' :: Prelude.Maybe [StringFilter],
    -- | A timestamp that indicates when the finding record was most recently
    -- updated.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    updatedAt :: Prelude.Maybe [DateFilter],
    -- | A list of user-defined name and value string pairs added to a finding.
    userDefinedFields :: Prelude.Maybe [MapFilter],
    -- | Provides the veracity of a finding.
    verificationState :: Prelude.Maybe [StringFilter],
    -- | Provides information about the status of the investigation into a
    -- finding.
    workflowStatus :: Prelude.Maybe [StringFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutomationRulesFindingFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'automationRulesFindingFilters_awsAccountId' - The Amazon Web Services account ID in which a finding was generated.
--
-- 'companyName', 'automationRulesFindingFilters_companyName' - The name of the company for the product that generated the finding. For
-- control-based findings, the company is Amazon Web Services.
--
-- 'complianceAssociatedStandardsId', 'automationRulesFindingFilters_complianceAssociatedStandardsId' - The unique identifier of a standard in which a control is enabled. This
-- field consists of the resource portion of the Amazon Resource Name (ARN)
-- returned for a standard in the
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_DescribeStandards.html DescribeStandards>
-- API response.
--
-- 'complianceSecurityControlId', 'automationRulesFindingFilters_complianceSecurityControlId' - The security control ID for which a finding was generated. Security
-- control IDs are the same across standards.
--
-- 'complianceStatus', 'automationRulesFindingFilters_complianceStatus' - The result of a security check. This field is only used for findings
-- generated from controls.
--
-- 'confidence', 'automationRulesFindingFilters_confidence' - The likelihood that a finding accurately identifies the behavior or
-- issue that it was intended to identify. @Confidence@ is scored on a
-- 0–100 basis using a ratio scale. A value of @0@ means 0 percent
-- confidence, and a value of @100@ means 100 percent confidence. For
-- example, a data exfiltration detection based on a statistical deviation
-- of network traffic has low confidence because an actual exfiltration
-- hasn\'t been verified. For more information, see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/asff-top-level-attributes.html#asff-confidence Confidence>
-- in the /Security Hub User Guide/.
--
-- 'createdAt', 'automationRulesFindingFilters_createdAt' - A timestamp that indicates when this finding record was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'criticality', 'automationRulesFindingFilters_criticality' - The level of importance that is assigned to the resources that are
-- associated with a finding. @Criticality@ is scored on a 0–100 basis,
-- using a ratio scale that supports only full integers. A score of @0@
-- means that the underlying resources have no criticality, and a score of
-- @100@ is reserved for the most critical resources. For more information,
-- see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/asff-top-level-attributes.html#asff-criticality Criticality>
-- in the /Security Hub User Guide/.
--
-- 'description', 'automationRulesFindingFilters_description' - A finding\'s description.
--
-- 'firstObservedAt', 'automationRulesFindingFilters_firstObservedAt' - A timestamp that indicates when the potential security issue captured by
-- a finding was first observed by the security findings product.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'generatorId', 'automationRulesFindingFilters_generatorId' - The identifier for the solution-specific component that generated a
-- finding.
--
-- 'id', 'automationRulesFindingFilters_id' - The product-specific identifier for a finding.
--
-- 'lastObservedAt', 'automationRulesFindingFilters_lastObservedAt' - A timestamp that indicates when the potential security issue captured by
-- a finding was most recently observed by the security findings product.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'noteText', 'automationRulesFindingFilters_noteText' - The text of a user-defined note that\'s added to a finding.
--
-- 'noteUpdatedAt', 'automationRulesFindingFilters_noteUpdatedAt' - The timestamp of when the note was updated. Uses the date-time format
-- specified in
-- <https://www.rfc-editor.org/rfc/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'noteUpdatedBy', 'automationRulesFindingFilters_noteUpdatedBy' - The principal that created a note.
--
-- 'productArn', 'automationRulesFindingFilters_productArn' - The Amazon Resource Name (ARN) for a third-party product that generated
-- a finding in Security Hub.
--
-- 'productName', 'automationRulesFindingFilters_productName' - Provides the name of the product that generated the finding. For
-- control-based findings, the product name is Security Hub.
--
-- 'recordState', 'automationRulesFindingFilters_recordState' - Provides the current state of a finding.
--
-- 'relatedFindingsId', 'automationRulesFindingFilters_relatedFindingsId' - The product-generated identifier for a related finding.
--
-- 'relatedFindingsProductArn', 'automationRulesFindingFilters_relatedFindingsProductArn' - The ARN for the product that generated a related finding.
--
-- 'resourceDetailsOther', 'automationRulesFindingFilters_resourceDetailsOther' - Custom fields and values about the resource that a finding pertains to.
--
-- 'resourceId', 'automationRulesFindingFilters_resourceId' - The identifier for the given resource type. For Amazon Web Services
-- resources that are identified by Amazon Resource Names (ARNs), this is
-- the ARN. For Amazon Web Services resources that lack ARNs, this is the
-- identifier as defined by the Amazon Web Service that created the
-- resource. For non-Amazon Web Services resources, this is a unique
-- identifier that is associated with the resource.
--
-- 'resourcePartition', 'automationRulesFindingFilters_resourcePartition' - The partition in which the resource that the finding pertains to is
-- located. A partition is a group of Amazon Web Services Regions. Each
-- Amazon Web Services account is scoped to one partition.
--
-- 'resourceRegion', 'automationRulesFindingFilters_resourceRegion' - The Amazon Web Services Region where the resource that a finding
-- pertains to is located.
--
-- 'resourceTags', 'automationRulesFindingFilters_resourceTags' - A list of Amazon Web Services tags associated with a resource at the
-- time the finding was processed.
--
-- 'resourceType', 'automationRulesFindingFilters_resourceType' - The type of resource that the finding pertains to.
--
-- 'severityLabel', 'automationRulesFindingFilters_severityLabel' - The severity value of the finding.
--
-- 'sourceUrl', 'automationRulesFindingFilters_sourceUrl' - Provides a URL that links to a page about the current finding in the
-- finding product.
--
-- 'title', 'automationRulesFindingFilters_title' - A finding\'s title.
--
-- 'type'', 'automationRulesFindingFilters_type' - One or more finding types in the format of
-- namespace\/category\/classifier that classify a finding. For a list of
-- namespaces, classifiers, and categories, see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-findings-format-type-taxonomy.html Types taxonomy for ASFF>
-- in the /Security Hub User Guide/.
--
-- 'updatedAt', 'automationRulesFindingFilters_updatedAt' - A timestamp that indicates when the finding record was most recently
-- updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'userDefinedFields', 'automationRulesFindingFilters_userDefinedFields' - A list of user-defined name and value string pairs added to a finding.
--
-- 'verificationState', 'automationRulesFindingFilters_verificationState' - Provides the veracity of a finding.
--
-- 'workflowStatus', 'automationRulesFindingFilters_workflowStatus' - Provides information about the status of the investigation into a
-- finding.
newAutomationRulesFindingFilters ::
  AutomationRulesFindingFilters
newAutomationRulesFindingFilters =
  AutomationRulesFindingFilters'
    { awsAccountId =
        Prelude.Nothing,
      companyName = Prelude.Nothing,
      complianceAssociatedStandardsId =
        Prelude.Nothing,
      complianceSecurityControlId =
        Prelude.Nothing,
      complianceStatus = Prelude.Nothing,
      confidence = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      criticality = Prelude.Nothing,
      description = Prelude.Nothing,
      firstObservedAt = Prelude.Nothing,
      generatorId = Prelude.Nothing,
      id = Prelude.Nothing,
      lastObservedAt = Prelude.Nothing,
      noteText = Prelude.Nothing,
      noteUpdatedAt = Prelude.Nothing,
      noteUpdatedBy = Prelude.Nothing,
      productArn = Prelude.Nothing,
      productName = Prelude.Nothing,
      recordState = Prelude.Nothing,
      relatedFindingsId = Prelude.Nothing,
      relatedFindingsProductArn = Prelude.Nothing,
      resourceDetailsOther = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourcePartition = Prelude.Nothing,
      resourceRegion = Prelude.Nothing,
      resourceTags = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      severityLabel = Prelude.Nothing,
      sourceUrl = Prelude.Nothing,
      title = Prelude.Nothing,
      type' = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      userDefinedFields = Prelude.Nothing,
      verificationState = Prelude.Nothing,
      workflowStatus = Prelude.Nothing
    }

-- | The Amazon Web Services account ID in which a finding was generated.
automationRulesFindingFilters_awsAccountId :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_awsAccountId = Lens.lens (\AutomationRulesFindingFilters' {awsAccountId} -> awsAccountId) (\s@AutomationRulesFindingFilters' {} a -> s {awsAccountId = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The name of the company for the product that generated the finding. For
-- control-based findings, the company is Amazon Web Services.
automationRulesFindingFilters_companyName :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_companyName = Lens.lens (\AutomationRulesFindingFilters' {companyName} -> companyName) (\s@AutomationRulesFindingFilters' {} a -> s {companyName = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of a standard in which a control is enabled. This
-- field consists of the resource portion of the Amazon Resource Name (ARN)
-- returned for a standard in the
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_DescribeStandards.html DescribeStandards>
-- API response.
automationRulesFindingFilters_complianceAssociatedStandardsId :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_complianceAssociatedStandardsId = Lens.lens (\AutomationRulesFindingFilters' {complianceAssociatedStandardsId} -> complianceAssociatedStandardsId) (\s@AutomationRulesFindingFilters' {} a -> s {complianceAssociatedStandardsId = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The security control ID for which a finding was generated. Security
-- control IDs are the same across standards.
automationRulesFindingFilters_complianceSecurityControlId :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_complianceSecurityControlId = Lens.lens (\AutomationRulesFindingFilters' {complianceSecurityControlId} -> complianceSecurityControlId) (\s@AutomationRulesFindingFilters' {} a -> s {complianceSecurityControlId = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The result of a security check. This field is only used for findings
-- generated from controls.
automationRulesFindingFilters_complianceStatus :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_complianceStatus = Lens.lens (\AutomationRulesFindingFilters' {complianceStatus} -> complianceStatus) (\s@AutomationRulesFindingFilters' {} a -> s {complianceStatus = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The likelihood that a finding accurately identifies the behavior or
-- issue that it was intended to identify. @Confidence@ is scored on a
-- 0–100 basis using a ratio scale. A value of @0@ means 0 percent
-- confidence, and a value of @100@ means 100 percent confidence. For
-- example, a data exfiltration detection based on a statistical deviation
-- of network traffic has low confidence because an actual exfiltration
-- hasn\'t been verified. For more information, see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/asff-top-level-attributes.html#asff-confidence Confidence>
-- in the /Security Hub User Guide/.
automationRulesFindingFilters_confidence :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [NumberFilter])
automationRulesFindingFilters_confidence = Lens.lens (\AutomationRulesFindingFilters' {confidence} -> confidence) (\s@AutomationRulesFindingFilters' {} a -> s {confidence = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | A timestamp that indicates when this finding record was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
automationRulesFindingFilters_createdAt :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [DateFilter])
automationRulesFindingFilters_createdAt = Lens.lens (\AutomationRulesFindingFilters' {createdAt} -> createdAt) (\s@AutomationRulesFindingFilters' {} a -> s {createdAt = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The level of importance that is assigned to the resources that are
-- associated with a finding. @Criticality@ is scored on a 0–100 basis,
-- using a ratio scale that supports only full integers. A score of @0@
-- means that the underlying resources have no criticality, and a score of
-- @100@ is reserved for the most critical resources. For more information,
-- see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/asff-top-level-attributes.html#asff-criticality Criticality>
-- in the /Security Hub User Guide/.
automationRulesFindingFilters_criticality :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [NumberFilter])
automationRulesFindingFilters_criticality = Lens.lens (\AutomationRulesFindingFilters' {criticality} -> criticality) (\s@AutomationRulesFindingFilters' {} a -> s {criticality = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | A finding\'s description.
automationRulesFindingFilters_description :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_description = Lens.lens (\AutomationRulesFindingFilters' {description} -> description) (\s@AutomationRulesFindingFilters' {} a -> s {description = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | A timestamp that indicates when the potential security issue captured by
-- a finding was first observed by the security findings product.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
automationRulesFindingFilters_firstObservedAt :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [DateFilter])
automationRulesFindingFilters_firstObservedAt = Lens.lens (\AutomationRulesFindingFilters' {firstObservedAt} -> firstObservedAt) (\s@AutomationRulesFindingFilters' {} a -> s {firstObservedAt = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the solution-specific component that generated a
-- finding.
automationRulesFindingFilters_generatorId :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_generatorId = Lens.lens (\AutomationRulesFindingFilters' {generatorId} -> generatorId) (\s@AutomationRulesFindingFilters' {} a -> s {generatorId = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The product-specific identifier for a finding.
automationRulesFindingFilters_id :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_id = Lens.lens (\AutomationRulesFindingFilters' {id} -> id) (\s@AutomationRulesFindingFilters' {} a -> s {id = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | A timestamp that indicates when the potential security issue captured by
-- a finding was most recently observed by the security findings product.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
automationRulesFindingFilters_lastObservedAt :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [DateFilter])
automationRulesFindingFilters_lastObservedAt = Lens.lens (\AutomationRulesFindingFilters' {lastObservedAt} -> lastObservedAt) (\s@AutomationRulesFindingFilters' {} a -> s {lastObservedAt = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The text of a user-defined note that\'s added to a finding.
automationRulesFindingFilters_noteText :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_noteText = Lens.lens (\AutomationRulesFindingFilters' {noteText} -> noteText) (\s@AutomationRulesFindingFilters' {} a -> s {noteText = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp of when the note was updated. Uses the date-time format
-- specified in
-- <https://www.rfc-editor.org/rfc/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
automationRulesFindingFilters_noteUpdatedAt :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [DateFilter])
automationRulesFindingFilters_noteUpdatedAt = Lens.lens (\AutomationRulesFindingFilters' {noteUpdatedAt} -> noteUpdatedAt) (\s@AutomationRulesFindingFilters' {} a -> s {noteUpdatedAt = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The principal that created a note.
automationRulesFindingFilters_noteUpdatedBy :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_noteUpdatedBy = Lens.lens (\AutomationRulesFindingFilters' {noteUpdatedBy} -> noteUpdatedBy) (\s@AutomationRulesFindingFilters' {} a -> s {noteUpdatedBy = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for a third-party product that generated
-- a finding in Security Hub.
automationRulesFindingFilters_productArn :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_productArn = Lens.lens (\AutomationRulesFindingFilters' {productArn} -> productArn) (\s@AutomationRulesFindingFilters' {} a -> s {productArn = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | Provides the name of the product that generated the finding. For
-- control-based findings, the product name is Security Hub.
automationRulesFindingFilters_productName :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_productName = Lens.lens (\AutomationRulesFindingFilters' {productName} -> productName) (\s@AutomationRulesFindingFilters' {} a -> s {productName = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | Provides the current state of a finding.
automationRulesFindingFilters_recordState :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_recordState = Lens.lens (\AutomationRulesFindingFilters' {recordState} -> recordState) (\s@AutomationRulesFindingFilters' {} a -> s {recordState = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The product-generated identifier for a related finding.
automationRulesFindingFilters_relatedFindingsId :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_relatedFindingsId = Lens.lens (\AutomationRulesFindingFilters' {relatedFindingsId} -> relatedFindingsId) (\s@AutomationRulesFindingFilters' {} a -> s {relatedFindingsId = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The ARN for the product that generated a related finding.
automationRulesFindingFilters_relatedFindingsProductArn :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_relatedFindingsProductArn = Lens.lens (\AutomationRulesFindingFilters' {relatedFindingsProductArn} -> relatedFindingsProductArn) (\s@AutomationRulesFindingFilters' {} a -> s {relatedFindingsProductArn = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | Custom fields and values about the resource that a finding pertains to.
automationRulesFindingFilters_resourceDetailsOther :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [MapFilter])
automationRulesFindingFilters_resourceDetailsOther = Lens.lens (\AutomationRulesFindingFilters' {resourceDetailsOther} -> resourceDetailsOther) (\s@AutomationRulesFindingFilters' {} a -> s {resourceDetailsOther = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the given resource type. For Amazon Web Services
-- resources that are identified by Amazon Resource Names (ARNs), this is
-- the ARN. For Amazon Web Services resources that lack ARNs, this is the
-- identifier as defined by the Amazon Web Service that created the
-- resource. For non-Amazon Web Services resources, this is a unique
-- identifier that is associated with the resource.
automationRulesFindingFilters_resourceId :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_resourceId = Lens.lens (\AutomationRulesFindingFilters' {resourceId} -> resourceId) (\s@AutomationRulesFindingFilters' {} a -> s {resourceId = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The partition in which the resource that the finding pertains to is
-- located. A partition is a group of Amazon Web Services Regions. Each
-- Amazon Web Services account is scoped to one partition.
automationRulesFindingFilters_resourcePartition :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_resourcePartition = Lens.lens (\AutomationRulesFindingFilters' {resourcePartition} -> resourcePartition) (\s@AutomationRulesFindingFilters' {} a -> s {resourcePartition = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services Region where the resource that a finding
-- pertains to is located.
automationRulesFindingFilters_resourceRegion :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_resourceRegion = Lens.lens (\AutomationRulesFindingFilters' {resourceRegion} -> resourceRegion) (\s@AutomationRulesFindingFilters' {} a -> s {resourceRegion = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | A list of Amazon Web Services tags associated with a resource at the
-- time the finding was processed.
automationRulesFindingFilters_resourceTags :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [MapFilter])
automationRulesFindingFilters_resourceTags = Lens.lens (\AutomationRulesFindingFilters' {resourceTags} -> resourceTags) (\s@AutomationRulesFindingFilters' {} a -> s {resourceTags = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The type of resource that the finding pertains to.
automationRulesFindingFilters_resourceType :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_resourceType = Lens.lens (\AutomationRulesFindingFilters' {resourceType} -> resourceType) (\s@AutomationRulesFindingFilters' {} a -> s {resourceType = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The severity value of the finding.
automationRulesFindingFilters_severityLabel :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_severityLabel = Lens.lens (\AutomationRulesFindingFilters' {severityLabel} -> severityLabel) (\s@AutomationRulesFindingFilters' {} a -> s {severityLabel = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | Provides a URL that links to a page about the current finding in the
-- finding product.
automationRulesFindingFilters_sourceUrl :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_sourceUrl = Lens.lens (\AutomationRulesFindingFilters' {sourceUrl} -> sourceUrl) (\s@AutomationRulesFindingFilters' {} a -> s {sourceUrl = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | A finding\'s title.
automationRulesFindingFilters_title :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_title = Lens.lens (\AutomationRulesFindingFilters' {title} -> title) (\s@AutomationRulesFindingFilters' {} a -> s {title = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | One or more finding types in the format of
-- namespace\/category\/classifier that classify a finding. For a list of
-- namespaces, classifiers, and categories, see
-- <https://docs.aws.amazon.com/securityhub/latest/userguide/securityhub-findings-format-type-taxonomy.html Types taxonomy for ASFF>
-- in the /Security Hub User Guide/.
automationRulesFindingFilters_type :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_type = Lens.lens (\AutomationRulesFindingFilters' {type'} -> type') (\s@AutomationRulesFindingFilters' {} a -> s {type' = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | A timestamp that indicates when the finding record was most recently
-- updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
automationRulesFindingFilters_updatedAt :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [DateFilter])
automationRulesFindingFilters_updatedAt = Lens.lens (\AutomationRulesFindingFilters' {updatedAt} -> updatedAt) (\s@AutomationRulesFindingFilters' {} a -> s {updatedAt = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | A list of user-defined name and value string pairs added to a finding.
automationRulesFindingFilters_userDefinedFields :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [MapFilter])
automationRulesFindingFilters_userDefinedFields = Lens.lens (\AutomationRulesFindingFilters' {userDefinedFields} -> userDefinedFields) (\s@AutomationRulesFindingFilters' {} a -> s {userDefinedFields = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | Provides the veracity of a finding.
automationRulesFindingFilters_verificationState :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_verificationState = Lens.lens (\AutomationRulesFindingFilters' {verificationState} -> verificationState) (\s@AutomationRulesFindingFilters' {} a -> s {verificationState = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | Provides information about the status of the investigation into a
-- finding.
automationRulesFindingFilters_workflowStatus :: Lens.Lens' AutomationRulesFindingFilters (Prelude.Maybe [StringFilter])
automationRulesFindingFilters_workflowStatus = Lens.lens (\AutomationRulesFindingFilters' {workflowStatus} -> workflowStatus) (\s@AutomationRulesFindingFilters' {} a -> s {workflowStatus = a} :: AutomationRulesFindingFilters) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AutomationRulesFindingFilters where
  parseJSON =
    Data.withObject
      "AutomationRulesFindingFilters"
      ( \x ->
          AutomationRulesFindingFilters'
            Prelude.<$> (x Data..:? "AwsAccountId" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CompanyName" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "ComplianceAssociatedStandardsId"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "ComplianceSecurityControlId"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "ComplianceStatus"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Confidence" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CreatedAt" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Criticality" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Description" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "FirstObservedAt"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "GeneratorId" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Id" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LastObservedAt" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "NoteText" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "NoteUpdatedAt" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "NoteUpdatedBy" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ProductArn" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ProductName" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RecordState" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "RelatedFindingsId"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "RelatedFindingsProductArn"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "ResourceDetailsOther"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ResourceId" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "ResourcePartition"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ResourceRegion" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ResourceTags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ResourceType" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SeverityLabel" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SourceUrl" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Title" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "UpdatedAt" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "UserDefinedFields"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "VerificationState"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "WorkflowStatus"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    AutomationRulesFindingFilters
  where
  hashWithSalt _salt AutomationRulesFindingFilters' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` companyName
      `Prelude.hashWithSalt` complianceAssociatedStandardsId
      `Prelude.hashWithSalt` complianceSecurityControlId
      `Prelude.hashWithSalt` complianceStatus
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` criticality
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` firstObservedAt
      `Prelude.hashWithSalt` generatorId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastObservedAt
      `Prelude.hashWithSalt` noteText
      `Prelude.hashWithSalt` noteUpdatedAt
      `Prelude.hashWithSalt` noteUpdatedBy
      `Prelude.hashWithSalt` productArn
      `Prelude.hashWithSalt` productName
      `Prelude.hashWithSalt` recordState
      `Prelude.hashWithSalt` relatedFindingsId
      `Prelude.hashWithSalt` relatedFindingsProductArn
      `Prelude.hashWithSalt` resourceDetailsOther
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourcePartition
      `Prelude.hashWithSalt` resourceRegion
      `Prelude.hashWithSalt` resourceTags
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` severityLabel
      `Prelude.hashWithSalt` sourceUrl
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` userDefinedFields
      `Prelude.hashWithSalt` verificationState
      `Prelude.hashWithSalt` workflowStatus

instance Prelude.NFData AutomationRulesFindingFilters where
  rnf AutomationRulesFindingFilters' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf companyName
      `Prelude.seq` Prelude.rnf complianceAssociatedStandardsId
      `Prelude.seq` Prelude.rnf complianceSecurityControlId
      `Prelude.seq` Prelude.rnf complianceStatus
      `Prelude.seq` Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf criticality
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf firstObservedAt
      `Prelude.seq` Prelude.rnf generatorId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastObservedAt
      `Prelude.seq` Prelude.rnf noteText
      `Prelude.seq` Prelude.rnf noteUpdatedAt
      `Prelude.seq` Prelude.rnf noteUpdatedBy
      `Prelude.seq` Prelude.rnf productArn
      `Prelude.seq` Prelude.rnf productName
      `Prelude.seq` Prelude.rnf recordState
      `Prelude.seq` Prelude.rnf relatedFindingsId
      `Prelude.seq` Prelude.rnf
        relatedFindingsProductArn
      `Prelude.seq` Prelude.rnf
        resourceDetailsOther
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf
        resourcePartition
      `Prelude.seq` Prelude.rnf
        resourceRegion
      `Prelude.seq` Prelude.rnf
        resourceTags
      `Prelude.seq` Prelude.rnf
        resourceType
      `Prelude.seq` Prelude.rnf
        severityLabel
      `Prelude.seq` Prelude.rnf
        sourceUrl
      `Prelude.seq` Prelude.rnf
        title
      `Prelude.seq` Prelude.rnf
        type'
      `Prelude.seq` Prelude.rnf
        updatedAt
      `Prelude.seq` Prelude.rnf
        userDefinedFields
      `Prelude.seq` Prelude.rnf
        verificationState
      `Prelude.seq` Prelude.rnf
        workflowStatus

instance Data.ToJSON AutomationRulesFindingFilters where
  toJSON AutomationRulesFindingFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AwsAccountId" Data..=) Prelude.<$> awsAccountId,
            ("CompanyName" Data..=) Prelude.<$> companyName,
            ("ComplianceAssociatedStandardsId" Data..=)
              Prelude.<$> complianceAssociatedStandardsId,
            ("ComplianceSecurityControlId" Data..=)
              Prelude.<$> complianceSecurityControlId,
            ("ComplianceStatus" Data..=)
              Prelude.<$> complianceStatus,
            ("Confidence" Data..=) Prelude.<$> confidence,
            ("CreatedAt" Data..=) Prelude.<$> createdAt,
            ("Criticality" Data..=) Prelude.<$> criticality,
            ("Description" Data..=) Prelude.<$> description,
            ("FirstObservedAt" Data..=)
              Prelude.<$> firstObservedAt,
            ("GeneratorId" Data..=) Prelude.<$> generatorId,
            ("Id" Data..=) Prelude.<$> id,
            ("LastObservedAt" Data..=)
              Prelude.<$> lastObservedAt,
            ("NoteText" Data..=) Prelude.<$> noteText,
            ("NoteUpdatedAt" Data..=) Prelude.<$> noteUpdatedAt,
            ("NoteUpdatedBy" Data..=) Prelude.<$> noteUpdatedBy,
            ("ProductArn" Data..=) Prelude.<$> productArn,
            ("ProductName" Data..=) Prelude.<$> productName,
            ("RecordState" Data..=) Prelude.<$> recordState,
            ("RelatedFindingsId" Data..=)
              Prelude.<$> relatedFindingsId,
            ("RelatedFindingsProductArn" Data..=)
              Prelude.<$> relatedFindingsProductArn,
            ("ResourceDetailsOther" Data..=)
              Prelude.<$> resourceDetailsOther,
            ("ResourceId" Data..=) Prelude.<$> resourceId,
            ("ResourcePartition" Data..=)
              Prelude.<$> resourcePartition,
            ("ResourceRegion" Data..=)
              Prelude.<$> resourceRegion,
            ("ResourceTags" Data..=) Prelude.<$> resourceTags,
            ("ResourceType" Data..=) Prelude.<$> resourceType,
            ("SeverityLabel" Data..=) Prelude.<$> severityLabel,
            ("SourceUrl" Data..=) Prelude.<$> sourceUrl,
            ("Title" Data..=) Prelude.<$> title,
            ("Type" Data..=) Prelude.<$> type',
            ("UpdatedAt" Data..=) Prelude.<$> updatedAt,
            ("UserDefinedFields" Data..=)
              Prelude.<$> userDefinedFields,
            ("VerificationState" Data..=)
              Prelude.<$> verificationState,
            ("WorkflowStatus" Data..=)
              Prelude.<$> workflowStatus
          ]
      )
