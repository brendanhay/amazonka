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
-- Module      : Amazonka.SecurityHub.Types.AwsSecurityFindingFilters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsSecurityFindingFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.BooleanFilter
import Amazonka.SecurityHub.Types.DateFilter
import Amazonka.SecurityHub.Types.IpFilter
import Amazonka.SecurityHub.Types.KeywordFilter
import Amazonka.SecurityHub.Types.MapFilter
import Amazonka.SecurityHub.Types.NumberFilter
import Amazonka.SecurityHub.Types.StringFilter

-- | A collection of attributes that are applied to all active Security
-- Hub-aggregated findings and that result in a subset of findings that are
-- included in this insight.
--
-- You can filter by up to 10 finding attributes. For each attribute, you
-- can provide up to 20 filter values.
--
-- /See:/ 'newAwsSecurityFindingFilters' smart constructor.
data AwsSecurityFindingFilters = AwsSecurityFindingFilters'
  { -- | The text of a note.
    noteText :: Prelude.Maybe [StringFilter],
    -- | The filesystem path of the malware that was observed.
    malwarePath :: Prelude.Maybe [StringFilter],
    -- | The canonical identifier for the given resource type.
    resourceId :: Prelude.Maybe [StringFilter],
    -- | The native severity as defined by the security-findings provider\'s
    -- solution that generated the finding.
    severityProduct :: Prelude.Maybe [NumberFilter],
    -- | The name of the solution (product) that generates findings.
    productName :: Prelude.Maybe [StringFilter],
    -- | The protocol of network-related information about a finding.
    networkProtocol :: Prelude.Maybe [StringFilter],
    -- | The destination IPv6 address of network-related information about a
    -- finding.
    networkDestinationIpV6 :: Prelude.Maybe [IpFilter],
    -- | The Amazon Web Services account ID that a finding is generated in.
    awsAccountId :: Prelude.Maybe [StringFilter],
    -- | The date\/time of the last observation of a threat intelligence
    -- indicator.
    threatIntelIndicatorLastObservedAt :: Prelude.Maybe [DateFilter],
    -- | Specifies the type of the resource that details are provided for.
    resourceType :: Prelude.Maybe [StringFilter],
    -- | The recommendation of what to do about the issue described in a finding.
    recommendationText :: Prelude.Maybe [StringFilter],
    -- | The level of importance assigned to the resources associated with the
    -- finding.
    --
    -- A score of 0 means that the underlying resources have no criticality,
    -- and a score of 100 is reserved for the most critical resources.
    criticality :: Prelude.Maybe [NumberFilter],
    -- | The Amazon Machine Image (AMI) ID of the instance.
    resourceAwsEc2InstanceImageId :: Prelude.Maybe [StringFilter],
    -- | The updated record state for the finding.
    recordState :: Prelude.Maybe [StringFilter],
    -- | The process ID.
    processPid :: Prelude.Maybe [NumberFilter],
    -- | The finding provider\'s original value for the severity.
    findingProviderFieldsSeverityOriginal :: Prelude.Maybe [StringFilter],
    -- | The source media access control (MAC) address of network-related
    -- information about a finding.
    networkSourceMac :: Prelude.Maybe [StringFilter],
    -- | A finding type in the format of @namespace\/category\/classifier@ that
    -- classifies a finding.
    type' :: Prelude.Maybe [StringFilter],
    -- | A data type where security-findings providers can include additional
    -- solution-specific details that aren\'t part of the defined
    -- @AwsSecurityFinding@ format.
    productFields :: Prelude.Maybe [MapFilter],
    -- | The name of the findings provider (company) that owns the solution
    -- (product) that generates findings.
    companyName :: Prelude.Maybe [StringFilter],
    -- | The value of a threat intelligence indicator.
    threatIntelIndicatorValue :: Prelude.Maybe [StringFilter],
    -- | The date\/time that the process was terminated.
    processTerminatedAt :: Prelude.Maybe [DateFilter],
    -- | The destination IPv4 address of network-related information about a
    -- finding.
    networkDestinationIpV4 :: Prelude.Maybe [IpFilter],
    -- | The canonical user ID of the owner of the S3 bucket.
    resourceAwsS3BucketOwnerId :: Prelude.Maybe [StringFilter],
    -- | The finding identifier of a related finding that is identified by the
    -- finding provider.
    findingProviderFieldsRelatedFindingsId :: Prelude.Maybe [StringFilter],
    -- | The name of an IAM user.
    resourceAwsIamUserUserName :: Prelude.Maybe [StringFilter],
    -- | The instance type of the instance.
    resourceAwsEc2InstanceType :: Prelude.Maybe [StringFilter],
    -- | The IPv4 addresses associated with the instance.
    resourceAwsEc2InstanceIpV4Addresses :: Prelude.Maybe [IpFilter],
    -- | The normalized severity of a finding.
    severityNormalized :: Prelude.Maybe [NumberFilter],
    -- | The date\/time that the container was started.
    resourceContainerLaunchedAt :: Prelude.Maybe [DateFilter],
    -- | The destination port of network-related information about a finding.
    networkDestinationPort :: Prelude.Maybe [NumberFilter],
    -- | The label of a finding\'s severity.
    severityLabel :: Prelude.Maybe [StringFilter],
    -- | A finding\'s confidence. Confidence is defined as the likelihood that a
    -- finding accurately identifies the behavior or issue that it was intended
    -- to identify.
    --
    -- Confidence is scored on a 0-100 basis using a ratio scale, where 0 means
    -- zero percent confidence and 100 means 100 percent confidence.
    confidence :: Prelude.Maybe [NumberFilter],
    -- | A list of name\/value string pairs associated with the finding. These
    -- are custom, user-defined fields added to a finding.
    userDefinedFields :: Prelude.Maybe [MapFilter],
    -- | The timestamp of when the note was updated.
    noteUpdatedAt :: Prelude.Maybe [DateFilter],
    -- | The display name of the owner of the S3 bucket.
    resourceAwsS3BucketOwnerName :: Prelude.Maybe [StringFilter],
    -- | The solution-generated identifier for a related finding.
    relatedFindingsId :: Prelude.Maybe [StringFilter],
    -- | The ARN of the solution that generated a related finding that is
    -- identified by the finding provider.
    findingProviderFieldsRelatedFindingsProductArn :: Prelude.Maybe [StringFilter],
    -- | The finding provider value for the finding confidence. Confidence is
    -- defined as the likelihood that a finding accurately identifies the
    -- behavior or issue that it was intended to identify.
    --
    -- Confidence is scored on a 0-100 basis using a ratio scale, where 0 means
    -- zero percent confidence and 100 means 100 percent confidence.
    findingProviderFieldsConfidence :: Prelude.Maybe [NumberFilter],
    -- | The creation date\/time of the IAM access key related to a finding.
    resourceAwsIamAccessKeyCreatedAt :: Prelude.Maybe [DateFilter],
    -- | The key name associated with the instance.
    resourceAwsEc2InstanceKeyName :: Prelude.Maybe [StringFilter],
    -- | The IAM profile ARN of the instance.
    resourceAwsEc2InstanceIamInstanceProfileArn :: Prelude.Maybe [StringFilter],
    -- | Exclusive to findings that are generated as the result of a check run
    -- against a specific rule in a supported standard, such as CIS Amazon Web
    -- Services Foundations. Contains security standard-related finding
    -- details.
    complianceStatus :: Prelude.Maybe [StringFilter],
    -- | The destination domain of network-related information about a finding.
    networkDestinationDomain :: Prelude.Maybe [StringFilter],
    -- | The source domain of network-related information about a finding.
    networkSourceDomain :: Prelude.Maybe [StringFilter],
    -- | The source of the threat intelligence.
    threatIntelIndicatorSource :: Prelude.Maybe [StringFilter],
    -- | The state of the malware that was observed.
    malwareState :: Prelude.Maybe [StringFilter],
    -- | A list of Amazon Web Services tags associated with a resource at the
    -- time the finding was processed.
    resourceTags :: Prelude.Maybe [MapFilter],
    -- | The canonical Amazon Web Services partition name that the Region is
    -- assigned to.
    resourcePartition :: Prelude.Maybe [StringFilter],
    -- | The security findings provider-specific identifier for a finding.
    id :: Prelude.Maybe [StringFilter],
    -- | A finding\'s description.
    description :: Prelude.Maybe [StringFilter],
    -- | The type of the malware that was observed.
    malwareType :: Prelude.Maybe [StringFilter],
    -- | The URL for more details from the source of the threat intelligence.
    threatIntelIndicatorSourceUrl :: Prelude.Maybe [StringFilter],
    -- | The name of the principal that is associated with an IAM access key.
    resourceAwsIamAccessKeyPrincipalName :: Prelude.Maybe [StringFilter],
    -- | The source IPv6 address of network-related information about a finding.
    networkSourceIpV6 :: Prelude.Maybe [IpFilter],
    -- | The Region from which the finding was generated.
    region :: Prelude.Maybe [StringFilter],
    -- | Indicates the direction of network traffic associated with a finding.
    networkDirection :: Prelude.Maybe [StringFilter],
    -- | A finding\'s title.
    title :: Prelude.Maybe [StringFilter],
    -- | An ISO8601-formatted timestamp that indicates when the security-findings
    -- provider first observed the potential security issue that a finding
    -- captured.
    firstObservedAt :: Prelude.Maybe [DateFilter],
    -- | The canonical Amazon Web Services external Region name where this
    -- resource is located.
    resourceRegion :: Prelude.Maybe [StringFilter],
    -- | A keyword for a finding.
    keyword :: Prelude.Maybe [KeywordFilter],
    -- | The ARN generated by Security Hub that uniquely identifies a third-party
    -- company (security findings provider) after this provider\'s product
    -- (solution that generates findings) is registered with Security Hub.
    productArn :: Prelude.Maybe [StringFilter],
    -- | The veracity of a finding.
    verificationState :: Prelude.Maybe [StringFilter],
    -- | The name of the container related to a finding.
    resourceContainerName :: Prelude.Maybe [StringFilter],
    -- | The category of a threat intelligence indicator.
    threatIntelIndicatorCategory :: Prelude.Maybe [StringFilter],
    -- | The user associated with the IAM access key related to a finding.
    resourceAwsIamAccessKeyUserName :: Prelude.Maybe [StringFilter],
    -- | The finding provider value for the level of importance assigned to the
    -- resources associated with the findings.
    --
    -- A score of 0 means that the underlying resources have no criticality,
    -- and a score of 100 is reserved for the most critical resources.
    findingProviderFieldsCriticality :: Prelude.Maybe [NumberFilter],
    -- | The identifier of the subnet that the instance was launched in.
    resourceAwsEc2InstanceSubnetId :: Prelude.Maybe [StringFilter],
    -- | An ISO8601-formatted timestamp that indicates when the security-findings
    -- provider most recently observed the potential security issue that a
    -- finding captured.
    lastObservedAt :: Prelude.Maybe [DateFilter],
    -- | The name of the malware that was observed.
    malwareName :: Prelude.Maybe [StringFilter],
    -- | The source IPv4 address of network-related information about a finding.
    networkSourceIpV4 :: Prelude.Maybe [IpFilter],
    -- | The name of the process.
    processName :: Prelude.Maybe [StringFilter],
    -- | The source port of network-related information about a finding.
    networkSourcePort :: Prelude.Maybe [NumberFilter],
    -- | The principal that created a note.
    noteUpdatedBy :: Prelude.Maybe [StringFilter],
    -- | The date\/time that the process was launched.
    processLaunchedAt :: Prelude.Maybe [DateFilter],
    -- | The type of a threat intelligence indicator.
    threatIntelIndicatorType :: Prelude.Maybe [StringFilter],
    -- | The status of the IAM access key related to a finding.
    resourceAwsIamAccessKeyStatus :: Prelude.Maybe [StringFilter],
    -- | The date and time the instance was launched.
    resourceAwsEc2InstanceLaunchedAt :: Prelude.Maybe [DateFilter],
    -- | The identifier of the VPC that the instance was launched in.
    resourceAwsEc2InstanceVpcId :: Prelude.Maybe [StringFilter],
    -- | One or more finding types that the finding provider assigned to the
    -- finding. Uses the format of @namespace\/category\/classifier@ that
    -- classify a finding.
    --
    -- Valid namespace values are: Software and Configuration Checks | TTPs |
    -- Effects | Unusual Behaviors | Sensitive Data Identifications
    findingProviderFieldsTypes :: Prelude.Maybe [StringFilter],
    -- | The status of the investigation into a finding. Allowed values are the
    -- following.
    --
    -- -   @NEW@ - The initial state of a finding, before it is reviewed.
    --
    --     Security Hub also resets the workflow status from @NOTIFIED@ or
    --     @RESOLVED@ to @NEW@ in the following cases:
    --
    --     -   @RecordState@ changes from @ARCHIVED@ to @ACTIVE@.
    --
    --     -   @Compliance.Status@ changes from @PASSED@ to either @WARNING@,
    --         @FAILED@, or @NOT_AVAILABLE@.
    --
    -- -   @NOTIFIED@ - Indicates that the resource owner has been notified
    --     about the security issue. Used when the initial reviewer is not the
    --     resource owner, and needs intervention from the resource owner.
    --
    --     If one of the following occurs, the workflow status is changed
    --     automatically from @NOTIFIED@ to @NEW@:
    --
    --     -   @RecordState@ changes from @ARCHIVED@ to @ACTIVE@.
    --
    --     -   @Compliance.Status@ changes from @PASSED@ to @FAILED@,
    --         @WARNING@, or @NOT_AVAILABLE@.
    --
    -- -   @SUPPRESSED@ - Indicates that you reviewed the finding and do not
    --     believe that any action is needed.
    --
    --     The workflow status of a @SUPPRESSED@ finding does not change if
    --     @RecordState@ changes from @ARCHIVED@ to @ACTIVE@.
    --
    -- -   @RESOLVED@ - The finding was reviewed and remediated and is now
    --     considered resolved.
    --
    --     The finding remains @RESOLVED@ unless one of the following occurs:
    --
    --     -   @RecordState@ changes from @ARCHIVED@ to @ACTIVE@.
    --
    --     -   @Compliance.Status@ changes from @PASSED@ to @FAILED@,
    --         @WARNING@, or @NOT_AVAILABLE@.
    --
    --     In those cases, the workflow status is automatically reset to @NEW@.
    --
    --     For findings from controls, if @Compliance.Status@ is @PASSED@, then
    --     Security Hub automatically sets the workflow status to @RESOLVED@.
    workflowStatus :: Prelude.Maybe [StringFilter],
    -- | A URL that links to a page about the current finding in the
    -- security-findings provider\'s solution.
    sourceUrl :: Prelude.Maybe [StringFilter],
    -- | The parent process ID.
    processParentPid :: Prelude.Maybe [NumberFilter],
    -- | An ISO8601-formatted timestamp that indicates when the security-findings
    -- provider captured the potential security issue that a finding captured.
    createdAt :: Prelude.Maybe [DateFilter],
    -- | The finding provider value for the severity label.
    findingProviderFieldsSeverityLabel :: Prelude.Maybe [StringFilter],
    -- | The IPv6 addresses associated with the instance.
    resourceAwsEc2InstanceIpV6Addresses :: Prelude.Maybe [IpFilter],
    -- | The workflow state of a finding.
    --
    -- Note that this field is deprecated. To search for a finding based on its
    -- workflow status, use @WorkflowStatus@.
    workflowState :: Prelude.Maybe [StringFilter],
    -- | The identifier for the solution-specific component (a discrete unit of
    -- logic) that generated a finding. In various security-findings
    -- providers\' solutions, this generator can be called a rule, a check, a
    -- detector, a plugin, etc.
    generatorId :: Prelude.Maybe [StringFilter],
    -- | An ISO8601-formatted timestamp that indicates when the security-findings
    -- provider last updated the finding record.
    updatedAt :: Prelude.Maybe [DateFilter],
    -- | The identifier of the image related to a finding.
    resourceContainerImageId :: Prelude.Maybe [StringFilter],
    -- | The name of the image related to a finding.
    resourceContainerImageName :: Prelude.Maybe [StringFilter],
    -- | The path to the process executable.
    processPath :: Prelude.Maybe [StringFilter],
    -- | Indicates whether or not sample findings are included in the filter
    -- results.
    sample :: Prelude.Maybe [BooleanFilter],
    -- | The ARN of the solution that generated a related finding.
    relatedFindingsProductArn :: Prelude.Maybe [StringFilter],
    -- | The details of a resource that doesn\'t have a specific subfield for the
    -- resource type defined.
    resourceDetailsOther :: Prelude.Maybe [MapFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsSecurityFindingFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'noteText', 'awsSecurityFindingFilters_noteText' - The text of a note.
--
-- 'malwarePath', 'awsSecurityFindingFilters_malwarePath' - The filesystem path of the malware that was observed.
--
-- 'resourceId', 'awsSecurityFindingFilters_resourceId' - The canonical identifier for the given resource type.
--
-- 'severityProduct', 'awsSecurityFindingFilters_severityProduct' - The native severity as defined by the security-findings provider\'s
-- solution that generated the finding.
--
-- 'productName', 'awsSecurityFindingFilters_productName' - The name of the solution (product) that generates findings.
--
-- 'networkProtocol', 'awsSecurityFindingFilters_networkProtocol' - The protocol of network-related information about a finding.
--
-- 'networkDestinationIpV6', 'awsSecurityFindingFilters_networkDestinationIpV6' - The destination IPv6 address of network-related information about a
-- finding.
--
-- 'awsAccountId', 'awsSecurityFindingFilters_awsAccountId' - The Amazon Web Services account ID that a finding is generated in.
--
-- 'threatIntelIndicatorLastObservedAt', 'awsSecurityFindingFilters_threatIntelIndicatorLastObservedAt' - The date\/time of the last observation of a threat intelligence
-- indicator.
--
-- 'resourceType', 'awsSecurityFindingFilters_resourceType' - Specifies the type of the resource that details are provided for.
--
-- 'recommendationText', 'awsSecurityFindingFilters_recommendationText' - The recommendation of what to do about the issue described in a finding.
--
-- 'criticality', 'awsSecurityFindingFilters_criticality' - The level of importance assigned to the resources associated with the
-- finding.
--
-- A score of 0 means that the underlying resources have no criticality,
-- and a score of 100 is reserved for the most critical resources.
--
-- 'resourceAwsEc2InstanceImageId', 'awsSecurityFindingFilters_resourceAwsEc2InstanceImageId' - The Amazon Machine Image (AMI) ID of the instance.
--
-- 'recordState', 'awsSecurityFindingFilters_recordState' - The updated record state for the finding.
--
-- 'processPid', 'awsSecurityFindingFilters_processPid' - The process ID.
--
-- 'findingProviderFieldsSeverityOriginal', 'awsSecurityFindingFilters_findingProviderFieldsSeverityOriginal' - The finding provider\'s original value for the severity.
--
-- 'networkSourceMac', 'awsSecurityFindingFilters_networkSourceMac' - The source media access control (MAC) address of network-related
-- information about a finding.
--
-- 'type'', 'awsSecurityFindingFilters_type' - A finding type in the format of @namespace\/category\/classifier@ that
-- classifies a finding.
--
-- 'productFields', 'awsSecurityFindingFilters_productFields' - A data type where security-findings providers can include additional
-- solution-specific details that aren\'t part of the defined
-- @AwsSecurityFinding@ format.
--
-- 'companyName', 'awsSecurityFindingFilters_companyName' - The name of the findings provider (company) that owns the solution
-- (product) that generates findings.
--
-- 'threatIntelIndicatorValue', 'awsSecurityFindingFilters_threatIntelIndicatorValue' - The value of a threat intelligence indicator.
--
-- 'processTerminatedAt', 'awsSecurityFindingFilters_processTerminatedAt' - The date\/time that the process was terminated.
--
-- 'networkDestinationIpV4', 'awsSecurityFindingFilters_networkDestinationIpV4' - The destination IPv4 address of network-related information about a
-- finding.
--
-- 'resourceAwsS3BucketOwnerId', 'awsSecurityFindingFilters_resourceAwsS3BucketOwnerId' - The canonical user ID of the owner of the S3 bucket.
--
-- 'findingProviderFieldsRelatedFindingsId', 'awsSecurityFindingFilters_findingProviderFieldsRelatedFindingsId' - The finding identifier of a related finding that is identified by the
-- finding provider.
--
-- 'resourceAwsIamUserUserName', 'awsSecurityFindingFilters_resourceAwsIamUserUserName' - The name of an IAM user.
--
-- 'resourceAwsEc2InstanceType', 'awsSecurityFindingFilters_resourceAwsEc2InstanceType' - The instance type of the instance.
--
-- 'resourceAwsEc2InstanceIpV4Addresses', 'awsSecurityFindingFilters_resourceAwsEc2InstanceIpV4Addresses' - The IPv4 addresses associated with the instance.
--
-- 'severityNormalized', 'awsSecurityFindingFilters_severityNormalized' - The normalized severity of a finding.
--
-- 'resourceContainerLaunchedAt', 'awsSecurityFindingFilters_resourceContainerLaunchedAt' - The date\/time that the container was started.
--
-- 'networkDestinationPort', 'awsSecurityFindingFilters_networkDestinationPort' - The destination port of network-related information about a finding.
--
-- 'severityLabel', 'awsSecurityFindingFilters_severityLabel' - The label of a finding\'s severity.
--
-- 'confidence', 'awsSecurityFindingFilters_confidence' - A finding\'s confidence. Confidence is defined as the likelihood that a
-- finding accurately identifies the behavior or issue that it was intended
-- to identify.
--
-- Confidence is scored on a 0-100 basis using a ratio scale, where 0 means
-- zero percent confidence and 100 means 100 percent confidence.
--
-- 'userDefinedFields', 'awsSecurityFindingFilters_userDefinedFields' - A list of name\/value string pairs associated with the finding. These
-- are custom, user-defined fields added to a finding.
--
-- 'noteUpdatedAt', 'awsSecurityFindingFilters_noteUpdatedAt' - The timestamp of when the note was updated.
--
-- 'resourceAwsS3BucketOwnerName', 'awsSecurityFindingFilters_resourceAwsS3BucketOwnerName' - The display name of the owner of the S3 bucket.
--
-- 'relatedFindingsId', 'awsSecurityFindingFilters_relatedFindingsId' - The solution-generated identifier for a related finding.
--
-- 'findingProviderFieldsRelatedFindingsProductArn', 'awsSecurityFindingFilters_findingProviderFieldsRelatedFindingsProductArn' - The ARN of the solution that generated a related finding that is
-- identified by the finding provider.
--
-- 'findingProviderFieldsConfidence', 'awsSecurityFindingFilters_findingProviderFieldsConfidence' - The finding provider value for the finding confidence. Confidence is
-- defined as the likelihood that a finding accurately identifies the
-- behavior or issue that it was intended to identify.
--
-- Confidence is scored on a 0-100 basis using a ratio scale, where 0 means
-- zero percent confidence and 100 means 100 percent confidence.
--
-- 'resourceAwsIamAccessKeyCreatedAt', 'awsSecurityFindingFilters_resourceAwsIamAccessKeyCreatedAt' - The creation date\/time of the IAM access key related to a finding.
--
-- 'resourceAwsEc2InstanceKeyName', 'awsSecurityFindingFilters_resourceAwsEc2InstanceKeyName' - The key name associated with the instance.
--
-- 'resourceAwsEc2InstanceIamInstanceProfileArn', 'awsSecurityFindingFilters_resourceAwsEc2InstanceIamInstanceProfileArn' - The IAM profile ARN of the instance.
--
-- 'complianceStatus', 'awsSecurityFindingFilters_complianceStatus' - Exclusive to findings that are generated as the result of a check run
-- against a specific rule in a supported standard, such as CIS Amazon Web
-- Services Foundations. Contains security standard-related finding
-- details.
--
-- 'networkDestinationDomain', 'awsSecurityFindingFilters_networkDestinationDomain' - The destination domain of network-related information about a finding.
--
-- 'networkSourceDomain', 'awsSecurityFindingFilters_networkSourceDomain' - The source domain of network-related information about a finding.
--
-- 'threatIntelIndicatorSource', 'awsSecurityFindingFilters_threatIntelIndicatorSource' - The source of the threat intelligence.
--
-- 'malwareState', 'awsSecurityFindingFilters_malwareState' - The state of the malware that was observed.
--
-- 'resourceTags', 'awsSecurityFindingFilters_resourceTags' - A list of Amazon Web Services tags associated with a resource at the
-- time the finding was processed.
--
-- 'resourcePartition', 'awsSecurityFindingFilters_resourcePartition' - The canonical Amazon Web Services partition name that the Region is
-- assigned to.
--
-- 'id', 'awsSecurityFindingFilters_id' - The security findings provider-specific identifier for a finding.
--
-- 'description', 'awsSecurityFindingFilters_description' - A finding\'s description.
--
-- 'malwareType', 'awsSecurityFindingFilters_malwareType' - The type of the malware that was observed.
--
-- 'threatIntelIndicatorSourceUrl', 'awsSecurityFindingFilters_threatIntelIndicatorSourceUrl' - The URL for more details from the source of the threat intelligence.
--
-- 'resourceAwsIamAccessKeyPrincipalName', 'awsSecurityFindingFilters_resourceAwsIamAccessKeyPrincipalName' - The name of the principal that is associated with an IAM access key.
--
-- 'networkSourceIpV6', 'awsSecurityFindingFilters_networkSourceIpV6' - The source IPv6 address of network-related information about a finding.
--
-- 'region', 'awsSecurityFindingFilters_region' - The Region from which the finding was generated.
--
-- 'networkDirection', 'awsSecurityFindingFilters_networkDirection' - Indicates the direction of network traffic associated with a finding.
--
-- 'title', 'awsSecurityFindingFilters_title' - A finding\'s title.
--
-- 'firstObservedAt', 'awsSecurityFindingFilters_firstObservedAt' - An ISO8601-formatted timestamp that indicates when the security-findings
-- provider first observed the potential security issue that a finding
-- captured.
--
-- 'resourceRegion', 'awsSecurityFindingFilters_resourceRegion' - The canonical Amazon Web Services external Region name where this
-- resource is located.
--
-- 'keyword', 'awsSecurityFindingFilters_keyword' - A keyword for a finding.
--
-- 'productArn', 'awsSecurityFindingFilters_productArn' - The ARN generated by Security Hub that uniquely identifies a third-party
-- company (security findings provider) after this provider\'s product
-- (solution that generates findings) is registered with Security Hub.
--
-- 'verificationState', 'awsSecurityFindingFilters_verificationState' - The veracity of a finding.
--
-- 'resourceContainerName', 'awsSecurityFindingFilters_resourceContainerName' - The name of the container related to a finding.
--
-- 'threatIntelIndicatorCategory', 'awsSecurityFindingFilters_threatIntelIndicatorCategory' - The category of a threat intelligence indicator.
--
-- 'resourceAwsIamAccessKeyUserName', 'awsSecurityFindingFilters_resourceAwsIamAccessKeyUserName' - The user associated with the IAM access key related to a finding.
--
-- 'findingProviderFieldsCriticality', 'awsSecurityFindingFilters_findingProviderFieldsCriticality' - The finding provider value for the level of importance assigned to the
-- resources associated with the findings.
--
-- A score of 0 means that the underlying resources have no criticality,
-- and a score of 100 is reserved for the most critical resources.
--
-- 'resourceAwsEc2InstanceSubnetId', 'awsSecurityFindingFilters_resourceAwsEc2InstanceSubnetId' - The identifier of the subnet that the instance was launched in.
--
-- 'lastObservedAt', 'awsSecurityFindingFilters_lastObservedAt' - An ISO8601-formatted timestamp that indicates when the security-findings
-- provider most recently observed the potential security issue that a
-- finding captured.
--
-- 'malwareName', 'awsSecurityFindingFilters_malwareName' - The name of the malware that was observed.
--
-- 'networkSourceIpV4', 'awsSecurityFindingFilters_networkSourceIpV4' - The source IPv4 address of network-related information about a finding.
--
-- 'processName', 'awsSecurityFindingFilters_processName' - The name of the process.
--
-- 'networkSourcePort', 'awsSecurityFindingFilters_networkSourcePort' - The source port of network-related information about a finding.
--
-- 'noteUpdatedBy', 'awsSecurityFindingFilters_noteUpdatedBy' - The principal that created a note.
--
-- 'processLaunchedAt', 'awsSecurityFindingFilters_processLaunchedAt' - The date\/time that the process was launched.
--
-- 'threatIntelIndicatorType', 'awsSecurityFindingFilters_threatIntelIndicatorType' - The type of a threat intelligence indicator.
--
-- 'resourceAwsIamAccessKeyStatus', 'awsSecurityFindingFilters_resourceAwsIamAccessKeyStatus' - The status of the IAM access key related to a finding.
--
-- 'resourceAwsEc2InstanceLaunchedAt', 'awsSecurityFindingFilters_resourceAwsEc2InstanceLaunchedAt' - The date and time the instance was launched.
--
-- 'resourceAwsEc2InstanceVpcId', 'awsSecurityFindingFilters_resourceAwsEc2InstanceVpcId' - The identifier of the VPC that the instance was launched in.
--
-- 'findingProviderFieldsTypes', 'awsSecurityFindingFilters_findingProviderFieldsTypes' - One or more finding types that the finding provider assigned to the
-- finding. Uses the format of @namespace\/category\/classifier@ that
-- classify a finding.
--
-- Valid namespace values are: Software and Configuration Checks | TTPs |
-- Effects | Unusual Behaviors | Sensitive Data Identifications
--
-- 'workflowStatus', 'awsSecurityFindingFilters_workflowStatus' - The status of the investigation into a finding. Allowed values are the
-- following.
--
-- -   @NEW@ - The initial state of a finding, before it is reviewed.
--
--     Security Hub also resets the workflow status from @NOTIFIED@ or
--     @RESOLVED@ to @NEW@ in the following cases:
--
--     -   @RecordState@ changes from @ARCHIVED@ to @ACTIVE@.
--
--     -   @Compliance.Status@ changes from @PASSED@ to either @WARNING@,
--         @FAILED@, or @NOT_AVAILABLE@.
--
-- -   @NOTIFIED@ - Indicates that the resource owner has been notified
--     about the security issue. Used when the initial reviewer is not the
--     resource owner, and needs intervention from the resource owner.
--
--     If one of the following occurs, the workflow status is changed
--     automatically from @NOTIFIED@ to @NEW@:
--
--     -   @RecordState@ changes from @ARCHIVED@ to @ACTIVE@.
--
--     -   @Compliance.Status@ changes from @PASSED@ to @FAILED@,
--         @WARNING@, or @NOT_AVAILABLE@.
--
-- -   @SUPPRESSED@ - Indicates that you reviewed the finding and do not
--     believe that any action is needed.
--
--     The workflow status of a @SUPPRESSED@ finding does not change if
--     @RecordState@ changes from @ARCHIVED@ to @ACTIVE@.
--
-- -   @RESOLVED@ - The finding was reviewed and remediated and is now
--     considered resolved.
--
--     The finding remains @RESOLVED@ unless one of the following occurs:
--
--     -   @RecordState@ changes from @ARCHIVED@ to @ACTIVE@.
--
--     -   @Compliance.Status@ changes from @PASSED@ to @FAILED@,
--         @WARNING@, or @NOT_AVAILABLE@.
--
--     In those cases, the workflow status is automatically reset to @NEW@.
--
--     For findings from controls, if @Compliance.Status@ is @PASSED@, then
--     Security Hub automatically sets the workflow status to @RESOLVED@.
--
-- 'sourceUrl', 'awsSecurityFindingFilters_sourceUrl' - A URL that links to a page about the current finding in the
-- security-findings provider\'s solution.
--
-- 'processParentPid', 'awsSecurityFindingFilters_processParentPid' - The parent process ID.
--
-- 'createdAt', 'awsSecurityFindingFilters_createdAt' - An ISO8601-formatted timestamp that indicates when the security-findings
-- provider captured the potential security issue that a finding captured.
--
-- 'findingProviderFieldsSeverityLabel', 'awsSecurityFindingFilters_findingProviderFieldsSeverityLabel' - The finding provider value for the severity label.
--
-- 'resourceAwsEc2InstanceIpV6Addresses', 'awsSecurityFindingFilters_resourceAwsEc2InstanceIpV6Addresses' - The IPv6 addresses associated with the instance.
--
-- 'workflowState', 'awsSecurityFindingFilters_workflowState' - The workflow state of a finding.
--
-- Note that this field is deprecated. To search for a finding based on its
-- workflow status, use @WorkflowStatus@.
--
-- 'generatorId', 'awsSecurityFindingFilters_generatorId' - The identifier for the solution-specific component (a discrete unit of
-- logic) that generated a finding. In various security-findings
-- providers\' solutions, this generator can be called a rule, a check, a
-- detector, a plugin, etc.
--
-- 'updatedAt', 'awsSecurityFindingFilters_updatedAt' - An ISO8601-formatted timestamp that indicates when the security-findings
-- provider last updated the finding record.
--
-- 'resourceContainerImageId', 'awsSecurityFindingFilters_resourceContainerImageId' - The identifier of the image related to a finding.
--
-- 'resourceContainerImageName', 'awsSecurityFindingFilters_resourceContainerImageName' - The name of the image related to a finding.
--
-- 'processPath', 'awsSecurityFindingFilters_processPath' - The path to the process executable.
--
-- 'sample', 'awsSecurityFindingFilters_sample' - Indicates whether or not sample findings are included in the filter
-- results.
--
-- 'relatedFindingsProductArn', 'awsSecurityFindingFilters_relatedFindingsProductArn' - The ARN of the solution that generated a related finding.
--
-- 'resourceDetailsOther', 'awsSecurityFindingFilters_resourceDetailsOther' - The details of a resource that doesn\'t have a specific subfield for the
-- resource type defined.
newAwsSecurityFindingFilters ::
  AwsSecurityFindingFilters
newAwsSecurityFindingFilters =
  AwsSecurityFindingFilters'
    { noteText =
        Prelude.Nothing,
      malwarePath = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      severityProduct = Prelude.Nothing,
      productName = Prelude.Nothing,
      networkProtocol = Prelude.Nothing,
      networkDestinationIpV6 = Prelude.Nothing,
      awsAccountId = Prelude.Nothing,
      threatIntelIndicatorLastObservedAt =
        Prelude.Nothing,
      resourceType = Prelude.Nothing,
      recommendationText = Prelude.Nothing,
      criticality = Prelude.Nothing,
      resourceAwsEc2InstanceImageId = Prelude.Nothing,
      recordState = Prelude.Nothing,
      processPid = Prelude.Nothing,
      findingProviderFieldsSeverityOriginal =
        Prelude.Nothing,
      networkSourceMac = Prelude.Nothing,
      type' = Prelude.Nothing,
      productFields = Prelude.Nothing,
      companyName = Prelude.Nothing,
      threatIntelIndicatorValue = Prelude.Nothing,
      processTerminatedAt = Prelude.Nothing,
      networkDestinationIpV4 = Prelude.Nothing,
      resourceAwsS3BucketOwnerId = Prelude.Nothing,
      findingProviderFieldsRelatedFindingsId =
        Prelude.Nothing,
      resourceAwsIamUserUserName = Prelude.Nothing,
      resourceAwsEc2InstanceType = Prelude.Nothing,
      resourceAwsEc2InstanceIpV4Addresses =
        Prelude.Nothing,
      severityNormalized = Prelude.Nothing,
      resourceContainerLaunchedAt = Prelude.Nothing,
      networkDestinationPort = Prelude.Nothing,
      severityLabel = Prelude.Nothing,
      confidence = Prelude.Nothing,
      userDefinedFields = Prelude.Nothing,
      noteUpdatedAt = Prelude.Nothing,
      resourceAwsS3BucketOwnerName = Prelude.Nothing,
      relatedFindingsId = Prelude.Nothing,
      findingProviderFieldsRelatedFindingsProductArn =
        Prelude.Nothing,
      findingProviderFieldsConfidence =
        Prelude.Nothing,
      resourceAwsIamAccessKeyCreatedAt =
        Prelude.Nothing,
      resourceAwsEc2InstanceKeyName = Prelude.Nothing,
      resourceAwsEc2InstanceIamInstanceProfileArn =
        Prelude.Nothing,
      complianceStatus = Prelude.Nothing,
      networkDestinationDomain = Prelude.Nothing,
      networkSourceDomain = Prelude.Nothing,
      threatIntelIndicatorSource = Prelude.Nothing,
      malwareState = Prelude.Nothing,
      resourceTags = Prelude.Nothing,
      resourcePartition = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      malwareType = Prelude.Nothing,
      threatIntelIndicatorSourceUrl = Prelude.Nothing,
      resourceAwsIamAccessKeyPrincipalName =
        Prelude.Nothing,
      networkSourceIpV6 = Prelude.Nothing,
      region = Prelude.Nothing,
      networkDirection = Prelude.Nothing,
      title = Prelude.Nothing,
      firstObservedAt = Prelude.Nothing,
      resourceRegion = Prelude.Nothing,
      keyword = Prelude.Nothing,
      productArn = Prelude.Nothing,
      verificationState = Prelude.Nothing,
      resourceContainerName = Prelude.Nothing,
      threatIntelIndicatorCategory = Prelude.Nothing,
      resourceAwsIamAccessKeyUserName =
        Prelude.Nothing,
      findingProviderFieldsCriticality =
        Prelude.Nothing,
      resourceAwsEc2InstanceSubnetId = Prelude.Nothing,
      lastObservedAt = Prelude.Nothing,
      malwareName = Prelude.Nothing,
      networkSourceIpV4 = Prelude.Nothing,
      processName = Prelude.Nothing,
      networkSourcePort = Prelude.Nothing,
      noteUpdatedBy = Prelude.Nothing,
      processLaunchedAt = Prelude.Nothing,
      threatIntelIndicatorType = Prelude.Nothing,
      resourceAwsIamAccessKeyStatus = Prelude.Nothing,
      resourceAwsEc2InstanceLaunchedAt =
        Prelude.Nothing,
      resourceAwsEc2InstanceVpcId = Prelude.Nothing,
      findingProviderFieldsTypes = Prelude.Nothing,
      workflowStatus = Prelude.Nothing,
      sourceUrl = Prelude.Nothing,
      processParentPid = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      findingProviderFieldsSeverityLabel =
        Prelude.Nothing,
      resourceAwsEc2InstanceIpV6Addresses =
        Prelude.Nothing,
      workflowState = Prelude.Nothing,
      generatorId = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      resourceContainerImageId = Prelude.Nothing,
      resourceContainerImageName = Prelude.Nothing,
      processPath = Prelude.Nothing,
      sample = Prelude.Nothing,
      relatedFindingsProductArn = Prelude.Nothing,
      resourceDetailsOther = Prelude.Nothing
    }

-- | The text of a note.
awsSecurityFindingFilters_noteText :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_noteText = Lens.lens (\AwsSecurityFindingFilters' {noteText} -> noteText) (\s@AwsSecurityFindingFilters' {} a -> s {noteText = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The filesystem path of the malware that was observed.
awsSecurityFindingFilters_malwarePath :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_malwarePath = Lens.lens (\AwsSecurityFindingFilters' {malwarePath} -> malwarePath) (\s@AwsSecurityFindingFilters' {} a -> s {malwarePath = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The canonical identifier for the given resource type.
awsSecurityFindingFilters_resourceId :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourceId = Lens.lens (\AwsSecurityFindingFilters' {resourceId} -> resourceId) (\s@AwsSecurityFindingFilters' {} a -> s {resourceId = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The native severity as defined by the security-findings provider\'s
-- solution that generated the finding.
awsSecurityFindingFilters_severityProduct :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [NumberFilter])
awsSecurityFindingFilters_severityProduct = Lens.lens (\AwsSecurityFindingFilters' {severityProduct} -> severityProduct) (\s@AwsSecurityFindingFilters' {} a -> s {severityProduct = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The name of the solution (product) that generates findings.
awsSecurityFindingFilters_productName :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_productName = Lens.lens (\AwsSecurityFindingFilters' {productName} -> productName) (\s@AwsSecurityFindingFilters' {} a -> s {productName = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The protocol of network-related information about a finding.
awsSecurityFindingFilters_networkProtocol :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_networkProtocol = Lens.lens (\AwsSecurityFindingFilters' {networkProtocol} -> networkProtocol) (\s@AwsSecurityFindingFilters' {} a -> s {networkProtocol = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The destination IPv6 address of network-related information about a
-- finding.
awsSecurityFindingFilters_networkDestinationIpV6 :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [IpFilter])
awsSecurityFindingFilters_networkDestinationIpV6 = Lens.lens (\AwsSecurityFindingFilters' {networkDestinationIpV6} -> networkDestinationIpV6) (\s@AwsSecurityFindingFilters' {} a -> s {networkDestinationIpV6 = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account ID that a finding is generated in.
awsSecurityFindingFilters_awsAccountId :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_awsAccountId = Lens.lens (\AwsSecurityFindingFilters' {awsAccountId} -> awsAccountId) (\s@AwsSecurityFindingFilters' {} a -> s {awsAccountId = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The date\/time of the last observation of a threat intelligence
-- indicator.
awsSecurityFindingFilters_threatIntelIndicatorLastObservedAt :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [DateFilter])
awsSecurityFindingFilters_threatIntelIndicatorLastObservedAt = Lens.lens (\AwsSecurityFindingFilters' {threatIntelIndicatorLastObservedAt} -> threatIntelIndicatorLastObservedAt) (\s@AwsSecurityFindingFilters' {} a -> s {threatIntelIndicatorLastObservedAt = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the type of the resource that details are provided for.
awsSecurityFindingFilters_resourceType :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourceType = Lens.lens (\AwsSecurityFindingFilters' {resourceType} -> resourceType) (\s@AwsSecurityFindingFilters' {} a -> s {resourceType = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The recommendation of what to do about the issue described in a finding.
awsSecurityFindingFilters_recommendationText :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_recommendationText = Lens.lens (\AwsSecurityFindingFilters' {recommendationText} -> recommendationText) (\s@AwsSecurityFindingFilters' {} a -> s {recommendationText = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The level of importance assigned to the resources associated with the
-- finding.
--
-- A score of 0 means that the underlying resources have no criticality,
-- and a score of 100 is reserved for the most critical resources.
awsSecurityFindingFilters_criticality :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [NumberFilter])
awsSecurityFindingFilters_criticality = Lens.lens (\AwsSecurityFindingFilters' {criticality} -> criticality) (\s@AwsSecurityFindingFilters' {} a -> s {criticality = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Machine Image (AMI) ID of the instance.
awsSecurityFindingFilters_resourceAwsEc2InstanceImageId :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourceAwsEc2InstanceImageId = Lens.lens (\AwsSecurityFindingFilters' {resourceAwsEc2InstanceImageId} -> resourceAwsEc2InstanceImageId) (\s@AwsSecurityFindingFilters' {} a -> s {resourceAwsEc2InstanceImageId = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The updated record state for the finding.
awsSecurityFindingFilters_recordState :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_recordState = Lens.lens (\AwsSecurityFindingFilters' {recordState} -> recordState) (\s@AwsSecurityFindingFilters' {} a -> s {recordState = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The process ID.
awsSecurityFindingFilters_processPid :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [NumberFilter])
awsSecurityFindingFilters_processPid = Lens.lens (\AwsSecurityFindingFilters' {processPid} -> processPid) (\s@AwsSecurityFindingFilters' {} a -> s {processPid = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The finding provider\'s original value for the severity.
awsSecurityFindingFilters_findingProviderFieldsSeverityOriginal :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_findingProviderFieldsSeverityOriginal = Lens.lens (\AwsSecurityFindingFilters' {findingProviderFieldsSeverityOriginal} -> findingProviderFieldsSeverityOriginal) (\s@AwsSecurityFindingFilters' {} a -> s {findingProviderFieldsSeverityOriginal = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The source media access control (MAC) address of network-related
-- information about a finding.
awsSecurityFindingFilters_networkSourceMac :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_networkSourceMac = Lens.lens (\AwsSecurityFindingFilters' {networkSourceMac} -> networkSourceMac) (\s@AwsSecurityFindingFilters' {} a -> s {networkSourceMac = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | A finding type in the format of @namespace\/category\/classifier@ that
-- classifies a finding.
awsSecurityFindingFilters_type :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_type = Lens.lens (\AwsSecurityFindingFilters' {type'} -> type') (\s@AwsSecurityFindingFilters' {} a -> s {type' = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | A data type where security-findings providers can include additional
-- solution-specific details that aren\'t part of the defined
-- @AwsSecurityFinding@ format.
awsSecurityFindingFilters_productFields :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [MapFilter])
awsSecurityFindingFilters_productFields = Lens.lens (\AwsSecurityFindingFilters' {productFields} -> productFields) (\s@AwsSecurityFindingFilters' {} a -> s {productFields = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The name of the findings provider (company) that owns the solution
-- (product) that generates findings.
awsSecurityFindingFilters_companyName :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_companyName = Lens.lens (\AwsSecurityFindingFilters' {companyName} -> companyName) (\s@AwsSecurityFindingFilters' {} a -> s {companyName = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The value of a threat intelligence indicator.
awsSecurityFindingFilters_threatIntelIndicatorValue :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_threatIntelIndicatorValue = Lens.lens (\AwsSecurityFindingFilters' {threatIntelIndicatorValue} -> threatIntelIndicatorValue) (\s@AwsSecurityFindingFilters' {} a -> s {threatIntelIndicatorValue = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The date\/time that the process was terminated.
awsSecurityFindingFilters_processTerminatedAt :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [DateFilter])
awsSecurityFindingFilters_processTerminatedAt = Lens.lens (\AwsSecurityFindingFilters' {processTerminatedAt} -> processTerminatedAt) (\s@AwsSecurityFindingFilters' {} a -> s {processTerminatedAt = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The destination IPv4 address of network-related information about a
-- finding.
awsSecurityFindingFilters_networkDestinationIpV4 :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [IpFilter])
awsSecurityFindingFilters_networkDestinationIpV4 = Lens.lens (\AwsSecurityFindingFilters' {networkDestinationIpV4} -> networkDestinationIpV4) (\s@AwsSecurityFindingFilters' {} a -> s {networkDestinationIpV4 = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The canonical user ID of the owner of the S3 bucket.
awsSecurityFindingFilters_resourceAwsS3BucketOwnerId :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourceAwsS3BucketOwnerId = Lens.lens (\AwsSecurityFindingFilters' {resourceAwsS3BucketOwnerId} -> resourceAwsS3BucketOwnerId) (\s@AwsSecurityFindingFilters' {} a -> s {resourceAwsS3BucketOwnerId = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The finding identifier of a related finding that is identified by the
-- finding provider.
awsSecurityFindingFilters_findingProviderFieldsRelatedFindingsId :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_findingProviderFieldsRelatedFindingsId = Lens.lens (\AwsSecurityFindingFilters' {findingProviderFieldsRelatedFindingsId} -> findingProviderFieldsRelatedFindingsId) (\s@AwsSecurityFindingFilters' {} a -> s {findingProviderFieldsRelatedFindingsId = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The name of an IAM user.
awsSecurityFindingFilters_resourceAwsIamUserUserName :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourceAwsIamUserUserName = Lens.lens (\AwsSecurityFindingFilters' {resourceAwsIamUserUserName} -> resourceAwsIamUserUserName) (\s@AwsSecurityFindingFilters' {} a -> s {resourceAwsIamUserUserName = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The instance type of the instance.
awsSecurityFindingFilters_resourceAwsEc2InstanceType :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourceAwsEc2InstanceType = Lens.lens (\AwsSecurityFindingFilters' {resourceAwsEc2InstanceType} -> resourceAwsEc2InstanceType) (\s@AwsSecurityFindingFilters' {} a -> s {resourceAwsEc2InstanceType = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The IPv4 addresses associated with the instance.
awsSecurityFindingFilters_resourceAwsEc2InstanceIpV4Addresses :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [IpFilter])
awsSecurityFindingFilters_resourceAwsEc2InstanceIpV4Addresses = Lens.lens (\AwsSecurityFindingFilters' {resourceAwsEc2InstanceIpV4Addresses} -> resourceAwsEc2InstanceIpV4Addresses) (\s@AwsSecurityFindingFilters' {} a -> s {resourceAwsEc2InstanceIpV4Addresses = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The normalized severity of a finding.
awsSecurityFindingFilters_severityNormalized :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [NumberFilter])
awsSecurityFindingFilters_severityNormalized = Lens.lens (\AwsSecurityFindingFilters' {severityNormalized} -> severityNormalized) (\s@AwsSecurityFindingFilters' {} a -> s {severityNormalized = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The date\/time that the container was started.
awsSecurityFindingFilters_resourceContainerLaunchedAt :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [DateFilter])
awsSecurityFindingFilters_resourceContainerLaunchedAt = Lens.lens (\AwsSecurityFindingFilters' {resourceContainerLaunchedAt} -> resourceContainerLaunchedAt) (\s@AwsSecurityFindingFilters' {} a -> s {resourceContainerLaunchedAt = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The destination port of network-related information about a finding.
awsSecurityFindingFilters_networkDestinationPort :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [NumberFilter])
awsSecurityFindingFilters_networkDestinationPort = Lens.lens (\AwsSecurityFindingFilters' {networkDestinationPort} -> networkDestinationPort) (\s@AwsSecurityFindingFilters' {} a -> s {networkDestinationPort = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The label of a finding\'s severity.
awsSecurityFindingFilters_severityLabel :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_severityLabel = Lens.lens (\AwsSecurityFindingFilters' {severityLabel} -> severityLabel) (\s@AwsSecurityFindingFilters' {} a -> s {severityLabel = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | A finding\'s confidence. Confidence is defined as the likelihood that a
-- finding accurately identifies the behavior or issue that it was intended
-- to identify.
--
-- Confidence is scored on a 0-100 basis using a ratio scale, where 0 means
-- zero percent confidence and 100 means 100 percent confidence.
awsSecurityFindingFilters_confidence :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [NumberFilter])
awsSecurityFindingFilters_confidence = Lens.lens (\AwsSecurityFindingFilters' {confidence} -> confidence) (\s@AwsSecurityFindingFilters' {} a -> s {confidence = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | A list of name\/value string pairs associated with the finding. These
-- are custom, user-defined fields added to a finding.
awsSecurityFindingFilters_userDefinedFields :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [MapFilter])
awsSecurityFindingFilters_userDefinedFields = Lens.lens (\AwsSecurityFindingFilters' {userDefinedFields} -> userDefinedFields) (\s@AwsSecurityFindingFilters' {} a -> s {userDefinedFields = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp of when the note was updated.
awsSecurityFindingFilters_noteUpdatedAt :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [DateFilter])
awsSecurityFindingFilters_noteUpdatedAt = Lens.lens (\AwsSecurityFindingFilters' {noteUpdatedAt} -> noteUpdatedAt) (\s@AwsSecurityFindingFilters' {} a -> s {noteUpdatedAt = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The display name of the owner of the S3 bucket.
awsSecurityFindingFilters_resourceAwsS3BucketOwnerName :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourceAwsS3BucketOwnerName = Lens.lens (\AwsSecurityFindingFilters' {resourceAwsS3BucketOwnerName} -> resourceAwsS3BucketOwnerName) (\s@AwsSecurityFindingFilters' {} a -> s {resourceAwsS3BucketOwnerName = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The solution-generated identifier for a related finding.
awsSecurityFindingFilters_relatedFindingsId :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_relatedFindingsId = Lens.lens (\AwsSecurityFindingFilters' {relatedFindingsId} -> relatedFindingsId) (\s@AwsSecurityFindingFilters' {} a -> s {relatedFindingsId = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the solution that generated a related finding that is
-- identified by the finding provider.
awsSecurityFindingFilters_findingProviderFieldsRelatedFindingsProductArn :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_findingProviderFieldsRelatedFindingsProductArn = Lens.lens (\AwsSecurityFindingFilters' {findingProviderFieldsRelatedFindingsProductArn} -> findingProviderFieldsRelatedFindingsProductArn) (\s@AwsSecurityFindingFilters' {} a -> s {findingProviderFieldsRelatedFindingsProductArn = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The finding provider value for the finding confidence. Confidence is
-- defined as the likelihood that a finding accurately identifies the
-- behavior or issue that it was intended to identify.
--
-- Confidence is scored on a 0-100 basis using a ratio scale, where 0 means
-- zero percent confidence and 100 means 100 percent confidence.
awsSecurityFindingFilters_findingProviderFieldsConfidence :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [NumberFilter])
awsSecurityFindingFilters_findingProviderFieldsConfidence = Lens.lens (\AwsSecurityFindingFilters' {findingProviderFieldsConfidence} -> findingProviderFieldsConfidence) (\s@AwsSecurityFindingFilters' {} a -> s {findingProviderFieldsConfidence = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The creation date\/time of the IAM access key related to a finding.
awsSecurityFindingFilters_resourceAwsIamAccessKeyCreatedAt :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [DateFilter])
awsSecurityFindingFilters_resourceAwsIamAccessKeyCreatedAt = Lens.lens (\AwsSecurityFindingFilters' {resourceAwsIamAccessKeyCreatedAt} -> resourceAwsIamAccessKeyCreatedAt) (\s@AwsSecurityFindingFilters' {} a -> s {resourceAwsIamAccessKeyCreatedAt = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The key name associated with the instance.
awsSecurityFindingFilters_resourceAwsEc2InstanceKeyName :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourceAwsEc2InstanceKeyName = Lens.lens (\AwsSecurityFindingFilters' {resourceAwsEc2InstanceKeyName} -> resourceAwsEc2InstanceKeyName) (\s@AwsSecurityFindingFilters' {} a -> s {resourceAwsEc2InstanceKeyName = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The IAM profile ARN of the instance.
awsSecurityFindingFilters_resourceAwsEc2InstanceIamInstanceProfileArn :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourceAwsEc2InstanceIamInstanceProfileArn = Lens.lens (\AwsSecurityFindingFilters' {resourceAwsEc2InstanceIamInstanceProfileArn} -> resourceAwsEc2InstanceIamInstanceProfileArn) (\s@AwsSecurityFindingFilters' {} a -> s {resourceAwsEc2InstanceIamInstanceProfileArn = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | Exclusive to findings that are generated as the result of a check run
-- against a specific rule in a supported standard, such as CIS Amazon Web
-- Services Foundations. Contains security standard-related finding
-- details.
awsSecurityFindingFilters_complianceStatus :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_complianceStatus = Lens.lens (\AwsSecurityFindingFilters' {complianceStatus} -> complianceStatus) (\s@AwsSecurityFindingFilters' {} a -> s {complianceStatus = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The destination domain of network-related information about a finding.
awsSecurityFindingFilters_networkDestinationDomain :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_networkDestinationDomain = Lens.lens (\AwsSecurityFindingFilters' {networkDestinationDomain} -> networkDestinationDomain) (\s@AwsSecurityFindingFilters' {} a -> s {networkDestinationDomain = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The source domain of network-related information about a finding.
awsSecurityFindingFilters_networkSourceDomain :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_networkSourceDomain = Lens.lens (\AwsSecurityFindingFilters' {networkSourceDomain} -> networkSourceDomain) (\s@AwsSecurityFindingFilters' {} a -> s {networkSourceDomain = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The source of the threat intelligence.
awsSecurityFindingFilters_threatIntelIndicatorSource :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_threatIntelIndicatorSource = Lens.lens (\AwsSecurityFindingFilters' {threatIntelIndicatorSource} -> threatIntelIndicatorSource) (\s@AwsSecurityFindingFilters' {} a -> s {threatIntelIndicatorSource = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The state of the malware that was observed.
awsSecurityFindingFilters_malwareState :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_malwareState = Lens.lens (\AwsSecurityFindingFilters' {malwareState} -> malwareState) (\s@AwsSecurityFindingFilters' {} a -> s {malwareState = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | A list of Amazon Web Services tags associated with a resource at the
-- time the finding was processed.
awsSecurityFindingFilters_resourceTags :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [MapFilter])
awsSecurityFindingFilters_resourceTags = Lens.lens (\AwsSecurityFindingFilters' {resourceTags} -> resourceTags) (\s@AwsSecurityFindingFilters' {} a -> s {resourceTags = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The canonical Amazon Web Services partition name that the Region is
-- assigned to.
awsSecurityFindingFilters_resourcePartition :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourcePartition = Lens.lens (\AwsSecurityFindingFilters' {resourcePartition} -> resourcePartition) (\s@AwsSecurityFindingFilters' {} a -> s {resourcePartition = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The security findings provider-specific identifier for a finding.
awsSecurityFindingFilters_id :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_id = Lens.lens (\AwsSecurityFindingFilters' {id} -> id) (\s@AwsSecurityFindingFilters' {} a -> s {id = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | A finding\'s description.
awsSecurityFindingFilters_description :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_description = Lens.lens (\AwsSecurityFindingFilters' {description} -> description) (\s@AwsSecurityFindingFilters' {} a -> s {description = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The type of the malware that was observed.
awsSecurityFindingFilters_malwareType :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_malwareType = Lens.lens (\AwsSecurityFindingFilters' {malwareType} -> malwareType) (\s@AwsSecurityFindingFilters' {} a -> s {malwareType = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The URL for more details from the source of the threat intelligence.
awsSecurityFindingFilters_threatIntelIndicatorSourceUrl :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_threatIntelIndicatorSourceUrl = Lens.lens (\AwsSecurityFindingFilters' {threatIntelIndicatorSourceUrl} -> threatIntelIndicatorSourceUrl) (\s@AwsSecurityFindingFilters' {} a -> s {threatIntelIndicatorSourceUrl = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The name of the principal that is associated with an IAM access key.
awsSecurityFindingFilters_resourceAwsIamAccessKeyPrincipalName :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourceAwsIamAccessKeyPrincipalName = Lens.lens (\AwsSecurityFindingFilters' {resourceAwsIamAccessKeyPrincipalName} -> resourceAwsIamAccessKeyPrincipalName) (\s@AwsSecurityFindingFilters' {} a -> s {resourceAwsIamAccessKeyPrincipalName = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The source IPv6 address of network-related information about a finding.
awsSecurityFindingFilters_networkSourceIpV6 :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [IpFilter])
awsSecurityFindingFilters_networkSourceIpV6 = Lens.lens (\AwsSecurityFindingFilters' {networkSourceIpV6} -> networkSourceIpV6) (\s@AwsSecurityFindingFilters' {} a -> s {networkSourceIpV6 = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The Region from which the finding was generated.
awsSecurityFindingFilters_region :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_region = Lens.lens (\AwsSecurityFindingFilters' {region} -> region) (\s@AwsSecurityFindingFilters' {} a -> s {region = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | Indicates the direction of network traffic associated with a finding.
awsSecurityFindingFilters_networkDirection :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_networkDirection = Lens.lens (\AwsSecurityFindingFilters' {networkDirection} -> networkDirection) (\s@AwsSecurityFindingFilters' {} a -> s {networkDirection = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | A finding\'s title.
awsSecurityFindingFilters_title :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_title = Lens.lens (\AwsSecurityFindingFilters' {title} -> title) (\s@AwsSecurityFindingFilters' {} a -> s {title = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | An ISO8601-formatted timestamp that indicates when the security-findings
-- provider first observed the potential security issue that a finding
-- captured.
awsSecurityFindingFilters_firstObservedAt :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [DateFilter])
awsSecurityFindingFilters_firstObservedAt = Lens.lens (\AwsSecurityFindingFilters' {firstObservedAt} -> firstObservedAt) (\s@AwsSecurityFindingFilters' {} a -> s {firstObservedAt = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The canonical Amazon Web Services external Region name where this
-- resource is located.
awsSecurityFindingFilters_resourceRegion :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourceRegion = Lens.lens (\AwsSecurityFindingFilters' {resourceRegion} -> resourceRegion) (\s@AwsSecurityFindingFilters' {} a -> s {resourceRegion = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | A keyword for a finding.
awsSecurityFindingFilters_keyword :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [KeywordFilter])
awsSecurityFindingFilters_keyword = Lens.lens (\AwsSecurityFindingFilters' {keyword} -> keyword) (\s@AwsSecurityFindingFilters' {} a -> s {keyword = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The ARN generated by Security Hub that uniquely identifies a third-party
-- company (security findings provider) after this provider\'s product
-- (solution that generates findings) is registered with Security Hub.
awsSecurityFindingFilters_productArn :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_productArn = Lens.lens (\AwsSecurityFindingFilters' {productArn} -> productArn) (\s@AwsSecurityFindingFilters' {} a -> s {productArn = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The veracity of a finding.
awsSecurityFindingFilters_verificationState :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_verificationState = Lens.lens (\AwsSecurityFindingFilters' {verificationState} -> verificationState) (\s@AwsSecurityFindingFilters' {} a -> s {verificationState = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The name of the container related to a finding.
awsSecurityFindingFilters_resourceContainerName :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourceContainerName = Lens.lens (\AwsSecurityFindingFilters' {resourceContainerName} -> resourceContainerName) (\s@AwsSecurityFindingFilters' {} a -> s {resourceContainerName = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The category of a threat intelligence indicator.
awsSecurityFindingFilters_threatIntelIndicatorCategory :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_threatIntelIndicatorCategory = Lens.lens (\AwsSecurityFindingFilters' {threatIntelIndicatorCategory} -> threatIntelIndicatorCategory) (\s@AwsSecurityFindingFilters' {} a -> s {threatIntelIndicatorCategory = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The user associated with the IAM access key related to a finding.
awsSecurityFindingFilters_resourceAwsIamAccessKeyUserName :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourceAwsIamAccessKeyUserName = Lens.lens (\AwsSecurityFindingFilters' {resourceAwsIamAccessKeyUserName} -> resourceAwsIamAccessKeyUserName) (\s@AwsSecurityFindingFilters' {} a -> s {resourceAwsIamAccessKeyUserName = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The finding provider value for the level of importance assigned to the
-- resources associated with the findings.
--
-- A score of 0 means that the underlying resources have no criticality,
-- and a score of 100 is reserved for the most critical resources.
awsSecurityFindingFilters_findingProviderFieldsCriticality :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [NumberFilter])
awsSecurityFindingFilters_findingProviderFieldsCriticality = Lens.lens (\AwsSecurityFindingFilters' {findingProviderFieldsCriticality} -> findingProviderFieldsCriticality) (\s@AwsSecurityFindingFilters' {} a -> s {findingProviderFieldsCriticality = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the subnet that the instance was launched in.
awsSecurityFindingFilters_resourceAwsEc2InstanceSubnetId :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourceAwsEc2InstanceSubnetId = Lens.lens (\AwsSecurityFindingFilters' {resourceAwsEc2InstanceSubnetId} -> resourceAwsEc2InstanceSubnetId) (\s@AwsSecurityFindingFilters' {} a -> s {resourceAwsEc2InstanceSubnetId = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | An ISO8601-formatted timestamp that indicates when the security-findings
-- provider most recently observed the potential security issue that a
-- finding captured.
awsSecurityFindingFilters_lastObservedAt :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [DateFilter])
awsSecurityFindingFilters_lastObservedAt = Lens.lens (\AwsSecurityFindingFilters' {lastObservedAt} -> lastObservedAt) (\s@AwsSecurityFindingFilters' {} a -> s {lastObservedAt = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The name of the malware that was observed.
awsSecurityFindingFilters_malwareName :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_malwareName = Lens.lens (\AwsSecurityFindingFilters' {malwareName} -> malwareName) (\s@AwsSecurityFindingFilters' {} a -> s {malwareName = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The source IPv4 address of network-related information about a finding.
awsSecurityFindingFilters_networkSourceIpV4 :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [IpFilter])
awsSecurityFindingFilters_networkSourceIpV4 = Lens.lens (\AwsSecurityFindingFilters' {networkSourceIpV4} -> networkSourceIpV4) (\s@AwsSecurityFindingFilters' {} a -> s {networkSourceIpV4 = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The name of the process.
awsSecurityFindingFilters_processName :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_processName = Lens.lens (\AwsSecurityFindingFilters' {processName} -> processName) (\s@AwsSecurityFindingFilters' {} a -> s {processName = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The source port of network-related information about a finding.
awsSecurityFindingFilters_networkSourcePort :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [NumberFilter])
awsSecurityFindingFilters_networkSourcePort = Lens.lens (\AwsSecurityFindingFilters' {networkSourcePort} -> networkSourcePort) (\s@AwsSecurityFindingFilters' {} a -> s {networkSourcePort = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The principal that created a note.
awsSecurityFindingFilters_noteUpdatedBy :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_noteUpdatedBy = Lens.lens (\AwsSecurityFindingFilters' {noteUpdatedBy} -> noteUpdatedBy) (\s@AwsSecurityFindingFilters' {} a -> s {noteUpdatedBy = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The date\/time that the process was launched.
awsSecurityFindingFilters_processLaunchedAt :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [DateFilter])
awsSecurityFindingFilters_processLaunchedAt = Lens.lens (\AwsSecurityFindingFilters' {processLaunchedAt} -> processLaunchedAt) (\s@AwsSecurityFindingFilters' {} a -> s {processLaunchedAt = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The type of a threat intelligence indicator.
awsSecurityFindingFilters_threatIntelIndicatorType :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_threatIntelIndicatorType = Lens.lens (\AwsSecurityFindingFilters' {threatIntelIndicatorType} -> threatIntelIndicatorType) (\s@AwsSecurityFindingFilters' {} a -> s {threatIntelIndicatorType = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The status of the IAM access key related to a finding.
awsSecurityFindingFilters_resourceAwsIamAccessKeyStatus :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourceAwsIamAccessKeyStatus = Lens.lens (\AwsSecurityFindingFilters' {resourceAwsIamAccessKeyStatus} -> resourceAwsIamAccessKeyStatus) (\s@AwsSecurityFindingFilters' {} a -> s {resourceAwsIamAccessKeyStatus = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The date and time the instance was launched.
awsSecurityFindingFilters_resourceAwsEc2InstanceLaunchedAt :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [DateFilter])
awsSecurityFindingFilters_resourceAwsEc2InstanceLaunchedAt = Lens.lens (\AwsSecurityFindingFilters' {resourceAwsEc2InstanceLaunchedAt} -> resourceAwsEc2InstanceLaunchedAt) (\s@AwsSecurityFindingFilters' {} a -> s {resourceAwsEc2InstanceLaunchedAt = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the VPC that the instance was launched in.
awsSecurityFindingFilters_resourceAwsEc2InstanceVpcId :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourceAwsEc2InstanceVpcId = Lens.lens (\AwsSecurityFindingFilters' {resourceAwsEc2InstanceVpcId} -> resourceAwsEc2InstanceVpcId) (\s@AwsSecurityFindingFilters' {} a -> s {resourceAwsEc2InstanceVpcId = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | One or more finding types that the finding provider assigned to the
-- finding. Uses the format of @namespace\/category\/classifier@ that
-- classify a finding.
--
-- Valid namespace values are: Software and Configuration Checks | TTPs |
-- Effects | Unusual Behaviors | Sensitive Data Identifications
awsSecurityFindingFilters_findingProviderFieldsTypes :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_findingProviderFieldsTypes = Lens.lens (\AwsSecurityFindingFilters' {findingProviderFieldsTypes} -> findingProviderFieldsTypes) (\s@AwsSecurityFindingFilters' {} a -> s {findingProviderFieldsTypes = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The status of the investigation into a finding. Allowed values are the
-- following.
--
-- -   @NEW@ - The initial state of a finding, before it is reviewed.
--
--     Security Hub also resets the workflow status from @NOTIFIED@ or
--     @RESOLVED@ to @NEW@ in the following cases:
--
--     -   @RecordState@ changes from @ARCHIVED@ to @ACTIVE@.
--
--     -   @Compliance.Status@ changes from @PASSED@ to either @WARNING@,
--         @FAILED@, or @NOT_AVAILABLE@.
--
-- -   @NOTIFIED@ - Indicates that the resource owner has been notified
--     about the security issue. Used when the initial reviewer is not the
--     resource owner, and needs intervention from the resource owner.
--
--     If one of the following occurs, the workflow status is changed
--     automatically from @NOTIFIED@ to @NEW@:
--
--     -   @RecordState@ changes from @ARCHIVED@ to @ACTIVE@.
--
--     -   @Compliance.Status@ changes from @PASSED@ to @FAILED@,
--         @WARNING@, or @NOT_AVAILABLE@.
--
-- -   @SUPPRESSED@ - Indicates that you reviewed the finding and do not
--     believe that any action is needed.
--
--     The workflow status of a @SUPPRESSED@ finding does not change if
--     @RecordState@ changes from @ARCHIVED@ to @ACTIVE@.
--
-- -   @RESOLVED@ - The finding was reviewed and remediated and is now
--     considered resolved.
--
--     The finding remains @RESOLVED@ unless one of the following occurs:
--
--     -   @RecordState@ changes from @ARCHIVED@ to @ACTIVE@.
--
--     -   @Compliance.Status@ changes from @PASSED@ to @FAILED@,
--         @WARNING@, or @NOT_AVAILABLE@.
--
--     In those cases, the workflow status is automatically reset to @NEW@.
--
--     For findings from controls, if @Compliance.Status@ is @PASSED@, then
--     Security Hub automatically sets the workflow status to @RESOLVED@.
awsSecurityFindingFilters_workflowStatus :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_workflowStatus = Lens.lens (\AwsSecurityFindingFilters' {workflowStatus} -> workflowStatus) (\s@AwsSecurityFindingFilters' {} a -> s {workflowStatus = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | A URL that links to a page about the current finding in the
-- security-findings provider\'s solution.
awsSecurityFindingFilters_sourceUrl :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_sourceUrl = Lens.lens (\AwsSecurityFindingFilters' {sourceUrl} -> sourceUrl) (\s@AwsSecurityFindingFilters' {} a -> s {sourceUrl = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The parent process ID.
awsSecurityFindingFilters_processParentPid :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [NumberFilter])
awsSecurityFindingFilters_processParentPid = Lens.lens (\AwsSecurityFindingFilters' {processParentPid} -> processParentPid) (\s@AwsSecurityFindingFilters' {} a -> s {processParentPid = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | An ISO8601-formatted timestamp that indicates when the security-findings
-- provider captured the potential security issue that a finding captured.
awsSecurityFindingFilters_createdAt :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [DateFilter])
awsSecurityFindingFilters_createdAt = Lens.lens (\AwsSecurityFindingFilters' {createdAt} -> createdAt) (\s@AwsSecurityFindingFilters' {} a -> s {createdAt = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The finding provider value for the severity label.
awsSecurityFindingFilters_findingProviderFieldsSeverityLabel :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_findingProviderFieldsSeverityLabel = Lens.lens (\AwsSecurityFindingFilters' {findingProviderFieldsSeverityLabel} -> findingProviderFieldsSeverityLabel) (\s@AwsSecurityFindingFilters' {} a -> s {findingProviderFieldsSeverityLabel = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The IPv6 addresses associated with the instance.
awsSecurityFindingFilters_resourceAwsEc2InstanceIpV6Addresses :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [IpFilter])
awsSecurityFindingFilters_resourceAwsEc2InstanceIpV6Addresses = Lens.lens (\AwsSecurityFindingFilters' {resourceAwsEc2InstanceIpV6Addresses} -> resourceAwsEc2InstanceIpV6Addresses) (\s@AwsSecurityFindingFilters' {} a -> s {resourceAwsEc2InstanceIpV6Addresses = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The workflow state of a finding.
--
-- Note that this field is deprecated. To search for a finding based on its
-- workflow status, use @WorkflowStatus@.
awsSecurityFindingFilters_workflowState :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_workflowState = Lens.lens (\AwsSecurityFindingFilters' {workflowState} -> workflowState) (\s@AwsSecurityFindingFilters' {} a -> s {workflowState = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the solution-specific component (a discrete unit of
-- logic) that generated a finding. In various security-findings
-- providers\' solutions, this generator can be called a rule, a check, a
-- detector, a plugin, etc.
awsSecurityFindingFilters_generatorId :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_generatorId = Lens.lens (\AwsSecurityFindingFilters' {generatorId} -> generatorId) (\s@AwsSecurityFindingFilters' {} a -> s {generatorId = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | An ISO8601-formatted timestamp that indicates when the security-findings
-- provider last updated the finding record.
awsSecurityFindingFilters_updatedAt :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [DateFilter])
awsSecurityFindingFilters_updatedAt = Lens.lens (\AwsSecurityFindingFilters' {updatedAt} -> updatedAt) (\s@AwsSecurityFindingFilters' {} a -> s {updatedAt = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the image related to a finding.
awsSecurityFindingFilters_resourceContainerImageId :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourceContainerImageId = Lens.lens (\AwsSecurityFindingFilters' {resourceContainerImageId} -> resourceContainerImageId) (\s@AwsSecurityFindingFilters' {} a -> s {resourceContainerImageId = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The name of the image related to a finding.
awsSecurityFindingFilters_resourceContainerImageName :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_resourceContainerImageName = Lens.lens (\AwsSecurityFindingFilters' {resourceContainerImageName} -> resourceContainerImageName) (\s@AwsSecurityFindingFilters' {} a -> s {resourceContainerImageName = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The path to the process executable.
awsSecurityFindingFilters_processPath :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_processPath = Lens.lens (\AwsSecurityFindingFilters' {processPath} -> processPath) (\s@AwsSecurityFindingFilters' {} a -> s {processPath = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether or not sample findings are included in the filter
-- results.
awsSecurityFindingFilters_sample :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [BooleanFilter])
awsSecurityFindingFilters_sample = Lens.lens (\AwsSecurityFindingFilters' {sample} -> sample) (\s@AwsSecurityFindingFilters' {} a -> s {sample = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the solution that generated a related finding.
awsSecurityFindingFilters_relatedFindingsProductArn :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [StringFilter])
awsSecurityFindingFilters_relatedFindingsProductArn = Lens.lens (\AwsSecurityFindingFilters' {relatedFindingsProductArn} -> relatedFindingsProductArn) (\s@AwsSecurityFindingFilters' {} a -> s {relatedFindingsProductArn = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

-- | The details of a resource that doesn\'t have a specific subfield for the
-- resource type defined.
awsSecurityFindingFilters_resourceDetailsOther :: Lens.Lens' AwsSecurityFindingFilters (Prelude.Maybe [MapFilter])
awsSecurityFindingFilters_resourceDetailsOther = Lens.lens (\AwsSecurityFindingFilters' {resourceDetailsOther} -> resourceDetailsOther) (\s@AwsSecurityFindingFilters' {} a -> s {resourceDetailsOther = a} :: AwsSecurityFindingFilters) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AwsSecurityFindingFilters where
  parseJSON =
    Data.withObject
      "AwsSecurityFindingFilters"
      ( \x ->
          AwsSecurityFindingFilters'
            Prelude.<$> (x Data..:? "NoteText" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MalwarePath" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ResourceId" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "SeverityProduct"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ProductName" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "NetworkProtocol"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "NetworkDestinationIpV6"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "AwsAccountId" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "ThreatIntelIndicatorLastObservedAt"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ResourceType" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "RecommendationText"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Criticality" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "ResourceAwsEc2InstanceImageId"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "RecordState" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ProcessPid" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "FindingProviderFieldsSeverityOriginal"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "NetworkSourceMac"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Type" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ProductFields" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CompanyName" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "ThreatIntelIndicatorValue"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ProcessTerminatedAt"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "NetworkDestinationIpV4"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ResourceAwsS3BucketOwnerId"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "FindingProviderFieldsRelatedFindingsId"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ResourceAwsIamUserUserName"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ResourceAwsEc2InstanceType"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ResourceAwsEc2InstanceIpV4Addresses"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "SeverityNormalized"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ResourceContainerLaunchedAt"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "NetworkDestinationPort"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SeverityLabel" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Confidence" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "UserDefinedFields"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "NoteUpdatedAt" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "ResourceAwsS3BucketOwnerName"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "RelatedFindingsId"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "FindingProviderFieldsRelatedFindingsProductArn"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "FindingProviderFieldsConfidence"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ResourceAwsIamAccessKeyCreatedAt"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ResourceAwsEc2InstanceKeyName"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "ResourceAwsEc2InstanceIamInstanceProfileArn"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ComplianceStatus"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "NetworkDestinationDomain"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "NetworkSourceDomain"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ThreatIntelIndicatorSource"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "MalwareState" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ResourceTags" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "ResourcePartition"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Id" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Description" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MalwareType" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "ThreatIntelIndicatorSourceUrl"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ResourceAwsIamAccessKeyPrincipalName"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "NetworkSourceIpV6"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Region" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "NetworkDirection"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Title" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "FirstObservedAt"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ResourceRegion" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Keyword" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ProductArn" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "VerificationState"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ResourceContainerName"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ThreatIntelIndicatorCategory"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ResourceAwsIamAccessKeyUserName"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "FindingProviderFieldsCriticality"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ResourceAwsEc2InstanceSubnetId"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "LastObservedAt" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MalwareName" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "NetworkSourceIpV4"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ProcessName" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "NetworkSourcePort"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "NoteUpdatedBy" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "ProcessLaunchedAt"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ThreatIntelIndicatorType"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ResourceAwsIamAccessKeyStatus"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ResourceAwsEc2InstanceLaunchedAt"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ResourceAwsEc2InstanceVpcId"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "FindingProviderFieldsTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "WorkflowStatus" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SourceUrl" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "ProcessParentPid"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CreatedAt" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "FindingProviderFieldsSeverityLabel"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ResourceAwsEc2InstanceIpV6Addresses"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "WorkflowState" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "GeneratorId" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "UpdatedAt" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "ResourceContainerImageId"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ResourceContainerImageName"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ProcessPath" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Sample" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "RelatedFindingsProductArn"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ResourceDetailsOther"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsSecurityFindingFilters where
  hashWithSalt _salt AwsSecurityFindingFilters' {..} =
    _salt `Prelude.hashWithSalt` noteText
      `Prelude.hashWithSalt` malwarePath
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` severityProduct
      `Prelude.hashWithSalt` productName
      `Prelude.hashWithSalt` networkProtocol
      `Prelude.hashWithSalt` networkDestinationIpV6
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` threatIntelIndicatorLastObservedAt
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` recommendationText
      `Prelude.hashWithSalt` criticality
      `Prelude.hashWithSalt` resourceAwsEc2InstanceImageId
      `Prelude.hashWithSalt` recordState
      `Prelude.hashWithSalt` processPid
      `Prelude.hashWithSalt` findingProviderFieldsSeverityOriginal
      `Prelude.hashWithSalt` networkSourceMac
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` productFields
      `Prelude.hashWithSalt` companyName
      `Prelude.hashWithSalt` threatIntelIndicatorValue
      `Prelude.hashWithSalt` processTerminatedAt
      `Prelude.hashWithSalt` networkDestinationIpV4
      `Prelude.hashWithSalt` resourceAwsS3BucketOwnerId
      `Prelude.hashWithSalt` findingProviderFieldsRelatedFindingsId
      `Prelude.hashWithSalt` resourceAwsIamUserUserName
      `Prelude.hashWithSalt` resourceAwsEc2InstanceType
      `Prelude.hashWithSalt` resourceAwsEc2InstanceIpV4Addresses
      `Prelude.hashWithSalt` severityNormalized
      `Prelude.hashWithSalt` resourceContainerLaunchedAt
      `Prelude.hashWithSalt` networkDestinationPort
      `Prelude.hashWithSalt` severityLabel
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` userDefinedFields
      `Prelude.hashWithSalt` noteUpdatedAt
      `Prelude.hashWithSalt` resourceAwsS3BucketOwnerName
      `Prelude.hashWithSalt` relatedFindingsId
      `Prelude.hashWithSalt` findingProviderFieldsRelatedFindingsProductArn
      `Prelude.hashWithSalt` findingProviderFieldsConfidence
      `Prelude.hashWithSalt` resourceAwsIamAccessKeyCreatedAt
      `Prelude.hashWithSalt` resourceAwsEc2InstanceKeyName
      `Prelude.hashWithSalt` resourceAwsEc2InstanceIamInstanceProfileArn
      `Prelude.hashWithSalt` complianceStatus
      `Prelude.hashWithSalt` networkDestinationDomain
      `Prelude.hashWithSalt` networkSourceDomain
      `Prelude.hashWithSalt` threatIntelIndicatorSource
      `Prelude.hashWithSalt` malwareState
      `Prelude.hashWithSalt` resourceTags
      `Prelude.hashWithSalt` resourcePartition
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` malwareType
      `Prelude.hashWithSalt` threatIntelIndicatorSourceUrl
      `Prelude.hashWithSalt` resourceAwsIamAccessKeyPrincipalName
      `Prelude.hashWithSalt` networkSourceIpV6
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` networkDirection
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` firstObservedAt
      `Prelude.hashWithSalt` resourceRegion
      `Prelude.hashWithSalt` keyword
      `Prelude.hashWithSalt` productArn
      `Prelude.hashWithSalt` verificationState
      `Prelude.hashWithSalt` resourceContainerName
      `Prelude.hashWithSalt` threatIntelIndicatorCategory
      `Prelude.hashWithSalt` resourceAwsIamAccessKeyUserName
      `Prelude.hashWithSalt` findingProviderFieldsCriticality
      `Prelude.hashWithSalt` resourceAwsEc2InstanceSubnetId
      `Prelude.hashWithSalt` lastObservedAt
      `Prelude.hashWithSalt` malwareName
      `Prelude.hashWithSalt` networkSourceIpV4
      `Prelude.hashWithSalt` processName
      `Prelude.hashWithSalt` networkSourcePort
      `Prelude.hashWithSalt` noteUpdatedBy
      `Prelude.hashWithSalt` processLaunchedAt
      `Prelude.hashWithSalt` threatIntelIndicatorType
      `Prelude.hashWithSalt` resourceAwsIamAccessKeyStatus
      `Prelude.hashWithSalt` resourceAwsEc2InstanceLaunchedAt
      `Prelude.hashWithSalt` resourceAwsEc2InstanceVpcId
      `Prelude.hashWithSalt` findingProviderFieldsTypes
      `Prelude.hashWithSalt` workflowStatus
      `Prelude.hashWithSalt` sourceUrl
      `Prelude.hashWithSalt` processParentPid
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` findingProviderFieldsSeverityLabel
      `Prelude.hashWithSalt` resourceAwsEc2InstanceIpV6Addresses
      `Prelude.hashWithSalt` workflowState
      `Prelude.hashWithSalt` generatorId
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` resourceContainerImageId
      `Prelude.hashWithSalt` resourceContainerImageName
      `Prelude.hashWithSalt` processPath
      `Prelude.hashWithSalt` sample
      `Prelude.hashWithSalt` relatedFindingsProductArn
      `Prelude.hashWithSalt` resourceDetailsOther

instance Prelude.NFData AwsSecurityFindingFilters where
  rnf AwsSecurityFindingFilters' {..} =
    Prelude.rnf noteText
      `Prelude.seq` Prelude.rnf malwarePath
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf severityProduct
      `Prelude.seq` Prelude.rnf productName
      `Prelude.seq` Prelude.rnf networkProtocol
      `Prelude.seq` Prelude.rnf networkDestinationIpV6
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf threatIntelIndicatorLastObservedAt
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf recommendationText
      `Prelude.seq` Prelude.rnf criticality
      `Prelude.seq` Prelude.rnf resourceAwsEc2InstanceImageId
      `Prelude.seq` Prelude.rnf recordState
      `Prelude.seq` Prelude.rnf processPid
      `Prelude.seq` Prelude.rnf
        findingProviderFieldsSeverityOriginal
      `Prelude.seq` Prelude.rnf networkSourceMac
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf productFields
      `Prelude.seq` Prelude.rnf companyName
      `Prelude.seq` Prelude.rnf
        threatIntelIndicatorValue
      `Prelude.seq` Prelude.rnf
        processTerminatedAt
      `Prelude.seq` Prelude.rnf
        networkDestinationIpV4
      `Prelude.seq` Prelude.rnf
        resourceAwsS3BucketOwnerId
      `Prelude.seq` Prelude.rnf
        findingProviderFieldsRelatedFindingsId
      `Prelude.seq` Prelude.rnf
        resourceAwsIamUserUserName
      `Prelude.seq` Prelude.rnf
        resourceAwsEc2InstanceType
      `Prelude.seq` Prelude.rnf
        resourceAwsEc2InstanceIpV4Addresses
      `Prelude.seq` Prelude.rnf
        severityNormalized
      `Prelude.seq` Prelude.rnf
        resourceContainerLaunchedAt
      `Prelude.seq` Prelude.rnf
        networkDestinationPort
      `Prelude.seq` Prelude.rnf
        severityLabel
      `Prelude.seq` Prelude.rnf
        confidence
      `Prelude.seq` Prelude.rnf
        userDefinedFields
      `Prelude.seq` Prelude.rnf
        noteUpdatedAt
      `Prelude.seq` Prelude.rnf
        resourceAwsS3BucketOwnerName
      `Prelude.seq` Prelude.rnf
        relatedFindingsId
      `Prelude.seq` Prelude.rnf
        findingProviderFieldsRelatedFindingsProductArn
      `Prelude.seq` Prelude.rnf
        findingProviderFieldsConfidence
      `Prelude.seq` Prelude.rnf
        resourceAwsIamAccessKeyCreatedAt
      `Prelude.seq` Prelude.rnf
        resourceAwsEc2InstanceKeyName
      `Prelude.seq` Prelude.rnf
        resourceAwsEc2InstanceIamInstanceProfileArn
      `Prelude.seq` Prelude.rnf
        complianceStatus
      `Prelude.seq` Prelude.rnf
        networkDestinationDomain
      `Prelude.seq` Prelude.rnf
        networkSourceDomain
      `Prelude.seq` Prelude.rnf
        threatIntelIndicatorSource
      `Prelude.seq` Prelude.rnf
        malwareState
      `Prelude.seq` Prelude.rnf
        resourceTags
      `Prelude.seq` Prelude.rnf
        resourcePartition
      `Prelude.seq` Prelude.rnf
        id
      `Prelude.seq` Prelude.rnf
        description
      `Prelude.seq` Prelude.rnf
        malwareType
      `Prelude.seq` Prelude.rnf
        threatIntelIndicatorSourceUrl
      `Prelude.seq` Prelude.rnf
        resourceAwsIamAccessKeyPrincipalName
      `Prelude.seq` Prelude.rnf
        networkSourceIpV6
      `Prelude.seq` Prelude.rnf
        region
      `Prelude.seq` Prelude.rnf
        networkDirection
      `Prelude.seq` Prelude.rnf
        title
      `Prelude.seq` Prelude.rnf
        firstObservedAt
      `Prelude.seq` Prelude.rnf
        resourceRegion
      `Prelude.seq` Prelude.rnf
        keyword
      `Prelude.seq` Prelude.rnf
        productArn
      `Prelude.seq` Prelude.rnf
        verificationState
      `Prelude.seq` Prelude.rnf
        resourceContainerName
      `Prelude.seq` Prelude.rnf
        threatIntelIndicatorCategory
      `Prelude.seq` Prelude.rnf
        resourceAwsIamAccessKeyUserName
      `Prelude.seq` Prelude.rnf
        findingProviderFieldsCriticality
      `Prelude.seq` Prelude.rnf
        resourceAwsEc2InstanceSubnetId
      `Prelude.seq` Prelude.rnf
        lastObservedAt
      `Prelude.seq` Prelude.rnf
        malwareName
      `Prelude.seq` Prelude.rnf
        networkSourceIpV4
      `Prelude.seq` Prelude.rnf
        processName
      `Prelude.seq` Prelude.rnf
        networkSourcePort
      `Prelude.seq` Prelude.rnf
        noteUpdatedBy
      `Prelude.seq` Prelude.rnf
        processLaunchedAt
      `Prelude.seq` Prelude.rnf
        threatIntelIndicatorType
      `Prelude.seq` Prelude.rnf
        resourceAwsIamAccessKeyStatus
      `Prelude.seq` Prelude.rnf
        resourceAwsEc2InstanceLaunchedAt
      `Prelude.seq` Prelude.rnf
        resourceAwsEc2InstanceVpcId
      `Prelude.seq` Prelude.rnf
        findingProviderFieldsTypes
      `Prelude.seq` Prelude.rnf
        workflowStatus
      `Prelude.seq` Prelude.rnf
        sourceUrl
      `Prelude.seq` Prelude.rnf
        processParentPid
      `Prelude.seq` Prelude.rnf
        createdAt
      `Prelude.seq` Prelude.rnf
        findingProviderFieldsSeverityLabel
      `Prelude.seq` Prelude.rnf
        resourceAwsEc2InstanceIpV6Addresses
      `Prelude.seq` Prelude.rnf
        workflowState
      `Prelude.seq` Prelude.rnf
        generatorId
      `Prelude.seq` Prelude.rnf
        updatedAt
      `Prelude.seq` Prelude.rnf
        resourceContainerImageId
      `Prelude.seq` Prelude.rnf
        resourceContainerImageName
      `Prelude.seq` Prelude.rnf
        processPath
      `Prelude.seq` Prelude.rnf
        sample
      `Prelude.seq` Prelude.rnf
        relatedFindingsProductArn
      `Prelude.seq` Prelude.rnf
        resourceDetailsOther

instance Data.ToJSON AwsSecurityFindingFilters where
  toJSON AwsSecurityFindingFilters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NoteText" Data..=) Prelude.<$> noteText,
            ("MalwarePath" Data..=) Prelude.<$> malwarePath,
            ("ResourceId" Data..=) Prelude.<$> resourceId,
            ("SeverityProduct" Data..=)
              Prelude.<$> severityProduct,
            ("ProductName" Data..=) Prelude.<$> productName,
            ("NetworkProtocol" Data..=)
              Prelude.<$> networkProtocol,
            ("NetworkDestinationIpV6" Data..=)
              Prelude.<$> networkDestinationIpV6,
            ("AwsAccountId" Data..=) Prelude.<$> awsAccountId,
            ("ThreatIntelIndicatorLastObservedAt" Data..=)
              Prelude.<$> threatIntelIndicatorLastObservedAt,
            ("ResourceType" Data..=) Prelude.<$> resourceType,
            ("RecommendationText" Data..=)
              Prelude.<$> recommendationText,
            ("Criticality" Data..=) Prelude.<$> criticality,
            ("ResourceAwsEc2InstanceImageId" Data..=)
              Prelude.<$> resourceAwsEc2InstanceImageId,
            ("RecordState" Data..=) Prelude.<$> recordState,
            ("ProcessPid" Data..=) Prelude.<$> processPid,
            ("FindingProviderFieldsSeverityOriginal" Data..=)
              Prelude.<$> findingProviderFieldsSeverityOriginal,
            ("NetworkSourceMac" Data..=)
              Prelude.<$> networkSourceMac,
            ("Type" Data..=) Prelude.<$> type',
            ("ProductFields" Data..=) Prelude.<$> productFields,
            ("CompanyName" Data..=) Prelude.<$> companyName,
            ("ThreatIntelIndicatorValue" Data..=)
              Prelude.<$> threatIntelIndicatorValue,
            ("ProcessTerminatedAt" Data..=)
              Prelude.<$> processTerminatedAt,
            ("NetworkDestinationIpV4" Data..=)
              Prelude.<$> networkDestinationIpV4,
            ("ResourceAwsS3BucketOwnerId" Data..=)
              Prelude.<$> resourceAwsS3BucketOwnerId,
            ("FindingProviderFieldsRelatedFindingsId" Data..=)
              Prelude.<$> findingProviderFieldsRelatedFindingsId,
            ("ResourceAwsIamUserUserName" Data..=)
              Prelude.<$> resourceAwsIamUserUserName,
            ("ResourceAwsEc2InstanceType" Data..=)
              Prelude.<$> resourceAwsEc2InstanceType,
            ("ResourceAwsEc2InstanceIpV4Addresses" Data..=)
              Prelude.<$> resourceAwsEc2InstanceIpV4Addresses,
            ("SeverityNormalized" Data..=)
              Prelude.<$> severityNormalized,
            ("ResourceContainerLaunchedAt" Data..=)
              Prelude.<$> resourceContainerLaunchedAt,
            ("NetworkDestinationPort" Data..=)
              Prelude.<$> networkDestinationPort,
            ("SeverityLabel" Data..=) Prelude.<$> severityLabel,
            ("Confidence" Data..=) Prelude.<$> confidence,
            ("UserDefinedFields" Data..=)
              Prelude.<$> userDefinedFields,
            ("NoteUpdatedAt" Data..=) Prelude.<$> noteUpdatedAt,
            ("ResourceAwsS3BucketOwnerName" Data..=)
              Prelude.<$> resourceAwsS3BucketOwnerName,
            ("RelatedFindingsId" Data..=)
              Prelude.<$> relatedFindingsId,
            ( "FindingProviderFieldsRelatedFindingsProductArn"
                Data..=
            )
              Prelude.<$> findingProviderFieldsRelatedFindingsProductArn,
            ("FindingProviderFieldsConfidence" Data..=)
              Prelude.<$> findingProviderFieldsConfidence,
            ("ResourceAwsIamAccessKeyCreatedAt" Data..=)
              Prelude.<$> resourceAwsIamAccessKeyCreatedAt,
            ("ResourceAwsEc2InstanceKeyName" Data..=)
              Prelude.<$> resourceAwsEc2InstanceKeyName,
            ( "ResourceAwsEc2InstanceIamInstanceProfileArn"
                Data..=
            )
              Prelude.<$> resourceAwsEc2InstanceIamInstanceProfileArn,
            ("ComplianceStatus" Data..=)
              Prelude.<$> complianceStatus,
            ("NetworkDestinationDomain" Data..=)
              Prelude.<$> networkDestinationDomain,
            ("NetworkSourceDomain" Data..=)
              Prelude.<$> networkSourceDomain,
            ("ThreatIntelIndicatorSource" Data..=)
              Prelude.<$> threatIntelIndicatorSource,
            ("MalwareState" Data..=) Prelude.<$> malwareState,
            ("ResourceTags" Data..=) Prelude.<$> resourceTags,
            ("ResourcePartition" Data..=)
              Prelude.<$> resourcePartition,
            ("Id" Data..=) Prelude.<$> id,
            ("Description" Data..=) Prelude.<$> description,
            ("MalwareType" Data..=) Prelude.<$> malwareType,
            ("ThreatIntelIndicatorSourceUrl" Data..=)
              Prelude.<$> threatIntelIndicatorSourceUrl,
            ("ResourceAwsIamAccessKeyPrincipalName" Data..=)
              Prelude.<$> resourceAwsIamAccessKeyPrincipalName,
            ("NetworkSourceIpV6" Data..=)
              Prelude.<$> networkSourceIpV6,
            ("Region" Data..=) Prelude.<$> region,
            ("NetworkDirection" Data..=)
              Prelude.<$> networkDirection,
            ("Title" Data..=) Prelude.<$> title,
            ("FirstObservedAt" Data..=)
              Prelude.<$> firstObservedAt,
            ("ResourceRegion" Data..=)
              Prelude.<$> resourceRegion,
            ("Keyword" Data..=) Prelude.<$> keyword,
            ("ProductArn" Data..=) Prelude.<$> productArn,
            ("VerificationState" Data..=)
              Prelude.<$> verificationState,
            ("ResourceContainerName" Data..=)
              Prelude.<$> resourceContainerName,
            ("ThreatIntelIndicatorCategory" Data..=)
              Prelude.<$> threatIntelIndicatorCategory,
            ("ResourceAwsIamAccessKeyUserName" Data..=)
              Prelude.<$> resourceAwsIamAccessKeyUserName,
            ("FindingProviderFieldsCriticality" Data..=)
              Prelude.<$> findingProviderFieldsCriticality,
            ("ResourceAwsEc2InstanceSubnetId" Data..=)
              Prelude.<$> resourceAwsEc2InstanceSubnetId,
            ("LastObservedAt" Data..=)
              Prelude.<$> lastObservedAt,
            ("MalwareName" Data..=) Prelude.<$> malwareName,
            ("NetworkSourceIpV4" Data..=)
              Prelude.<$> networkSourceIpV4,
            ("ProcessName" Data..=) Prelude.<$> processName,
            ("NetworkSourcePort" Data..=)
              Prelude.<$> networkSourcePort,
            ("NoteUpdatedBy" Data..=) Prelude.<$> noteUpdatedBy,
            ("ProcessLaunchedAt" Data..=)
              Prelude.<$> processLaunchedAt,
            ("ThreatIntelIndicatorType" Data..=)
              Prelude.<$> threatIntelIndicatorType,
            ("ResourceAwsIamAccessKeyStatus" Data..=)
              Prelude.<$> resourceAwsIamAccessKeyStatus,
            ("ResourceAwsEc2InstanceLaunchedAt" Data..=)
              Prelude.<$> resourceAwsEc2InstanceLaunchedAt,
            ("ResourceAwsEc2InstanceVpcId" Data..=)
              Prelude.<$> resourceAwsEc2InstanceVpcId,
            ("FindingProviderFieldsTypes" Data..=)
              Prelude.<$> findingProviderFieldsTypes,
            ("WorkflowStatus" Data..=)
              Prelude.<$> workflowStatus,
            ("SourceUrl" Data..=) Prelude.<$> sourceUrl,
            ("ProcessParentPid" Data..=)
              Prelude.<$> processParentPid,
            ("CreatedAt" Data..=) Prelude.<$> createdAt,
            ("FindingProviderFieldsSeverityLabel" Data..=)
              Prelude.<$> findingProviderFieldsSeverityLabel,
            ("ResourceAwsEc2InstanceIpV6Addresses" Data..=)
              Prelude.<$> resourceAwsEc2InstanceIpV6Addresses,
            ("WorkflowState" Data..=) Prelude.<$> workflowState,
            ("GeneratorId" Data..=) Prelude.<$> generatorId,
            ("UpdatedAt" Data..=) Prelude.<$> updatedAt,
            ("ResourceContainerImageId" Data..=)
              Prelude.<$> resourceContainerImageId,
            ("ResourceContainerImageName" Data..=)
              Prelude.<$> resourceContainerImageName,
            ("ProcessPath" Data..=) Prelude.<$> processPath,
            ("Sample" Data..=) Prelude.<$> sample,
            ("RelatedFindingsProductArn" Data..=)
              Prelude.<$> relatedFindingsProductArn,
            ("ResourceDetailsOther" Data..=)
              Prelude.<$> resourceDetailsOther
          ]
      )
