{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.Sum where

import Network.AWS.Prelude

data AccessLevelFilterKey
  = Account
  | Role
  | User
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AccessLevelFilterKey where
    parser = takeLowerText >>= \case
        "account" -> pure Account
        "role" -> pure Role
        "user" -> pure User
        e -> fromTextError $ "Failure parsing AccessLevelFilterKey from value: '" <> e
           <> "'. Accepted values: account, role, user"

instance ToText AccessLevelFilterKey where
    toText = \case
        Account -> "Account"
        Role -> "Role"
        User -> "User"

instance Hashable     AccessLevelFilterKey
instance NFData       AccessLevelFilterKey
instance ToByteString AccessLevelFilterKey
instance ToQuery      AccessLevelFilterKey
instance ToHeader     AccessLevelFilterKey

instance ToJSON AccessLevelFilterKey where
    toJSON = toJSONText

data AccessStatus
  = Disabled
  | Enabled
  | UnderChange
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AccessStatus where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        "under_change" -> pure UnderChange
        e -> fromTextError $ "Failure parsing AccessStatus from value: '" <> e
           <> "'. Accepted values: disabled, enabled, under_change"

instance ToText AccessStatus where
    toText = \case
        Disabled -> "DISABLED"
        Enabled -> "ENABLED"
        UnderChange -> "UNDER_CHANGE"

instance Hashable     AccessStatus
instance NFData       AccessStatus
instance ToByteString AccessStatus
instance ToQuery      AccessStatus
instance ToHeader     AccessStatus

instance FromJSON AccessStatus where
    parseJSON = parseJSONText "AccessStatus"

data ChangeAction
  = Add
  | Modify
  | Remove
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChangeAction where
    parser = takeLowerText >>= \case
        "add" -> pure Add
        "modify" -> pure Modify
        "remove" -> pure Remove
        e -> fromTextError $ "Failure parsing ChangeAction from value: '" <> e
           <> "'. Accepted values: add, modify, remove"

instance ToText ChangeAction where
    toText = \case
        Add -> "ADD"
        Modify -> "MODIFY"
        Remove -> "REMOVE"

instance Hashable     ChangeAction
instance NFData       ChangeAction
instance ToByteString ChangeAction
instance ToQuery      ChangeAction
instance ToHeader     ChangeAction

instance FromJSON ChangeAction where
    parseJSON = parseJSONText "ChangeAction"

data CopyOption =
  CopyTags
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CopyOption where
    parser = takeLowerText >>= \case
        "copytags" -> pure CopyTags
        e -> fromTextError $ "Failure parsing CopyOption from value: '" <> e
           <> "'. Accepted values: copytags"

instance ToText CopyOption where
    toText = \case
        CopyTags -> "CopyTags"

instance Hashable     CopyOption
instance NFData       CopyOption
instance ToByteString CopyOption
instance ToQuery      CopyOption
instance ToHeader     CopyOption

instance ToJSON CopyOption where
    toJSON = toJSONText

data CopyProductStatus
  = CPSFailed
  | CPSInProgress
  | CPSSucceeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CopyProductStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure CPSFailed
        "in_progress" -> pure CPSInProgress
        "succeeded" -> pure CPSSucceeded
        e -> fromTextError $ "Failure parsing CopyProductStatus from value: '" <> e
           <> "'. Accepted values: failed, in_progress, succeeded"

instance ToText CopyProductStatus where
    toText = \case
        CPSFailed -> "FAILED"
        CPSInProgress -> "IN_PROGRESS"
        CPSSucceeded -> "SUCCEEDED"

instance Hashable     CopyProductStatus
instance NFData       CopyProductStatus
instance ToByteString CopyProductStatus
instance ToQuery      CopyProductStatus
instance ToHeader     CopyProductStatus

instance FromJSON CopyProductStatus where
    parseJSON = parseJSONText "CopyProductStatus"

data EvaluationType
  = Dynamic
  | Static
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EvaluationType where
    parser = takeLowerText >>= \case
        "dynamic" -> pure Dynamic
        "static" -> pure Static
        e -> fromTextError $ "Failure parsing EvaluationType from value: '" <> e
           <> "'. Accepted values: dynamic, static"

instance ToText EvaluationType where
    toText = \case
        Dynamic -> "DYNAMIC"
        Static -> "STATIC"

instance Hashable     EvaluationType
instance NFData       EvaluationType
instance ToByteString EvaluationType
instance ToQuery      EvaluationType
instance ToHeader     EvaluationType

instance FromJSON EvaluationType where
    parseJSON = parseJSONText "EvaluationType"

data OrganizationNodeType
  = ONTAccount
  | ONTOrganization
  | ONTOrganizationalUnit
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OrganizationNodeType where
    parser = takeLowerText >>= \case
        "account" -> pure ONTAccount
        "organization" -> pure ONTOrganization
        "organizational_unit" -> pure ONTOrganizationalUnit
        e -> fromTextError $ "Failure parsing OrganizationNodeType from value: '" <> e
           <> "'. Accepted values: account, organization, organizational_unit"

instance ToText OrganizationNodeType where
    toText = \case
        ONTAccount -> "ACCOUNT"
        ONTOrganization -> "ORGANIZATION"
        ONTOrganizationalUnit -> "ORGANIZATIONAL_UNIT"

instance Hashable     OrganizationNodeType
instance NFData       OrganizationNodeType
instance ToByteString OrganizationNodeType
instance ToQuery      OrganizationNodeType
instance ToHeader     OrganizationNodeType

instance ToJSON OrganizationNodeType where
    toJSON = toJSONText

instance FromJSON OrganizationNodeType where
    parseJSON = parseJSONText "OrganizationNodeType"

data PortfolioShareType
  = AWSOrganizations
  | AWSServicecatalog
  | Imported
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PortfolioShareType where
    parser = takeLowerText >>= \case
        "aws_organizations" -> pure AWSOrganizations
        "aws_servicecatalog" -> pure AWSServicecatalog
        "imported" -> pure Imported
        e -> fromTextError $ "Failure parsing PortfolioShareType from value: '" <> e
           <> "'. Accepted values: aws_organizations, aws_servicecatalog, imported"

instance ToText PortfolioShareType where
    toText = \case
        AWSOrganizations -> "AWS_ORGANIZATIONS"
        AWSServicecatalog -> "AWS_SERVICECATALOG"
        Imported -> "IMPORTED"

instance Hashable     PortfolioShareType
instance NFData       PortfolioShareType
instance ToByteString PortfolioShareType
instance ToQuery      PortfolioShareType
instance ToHeader     PortfolioShareType

instance ToJSON PortfolioShareType where
    toJSON = toJSONText

data PrincipalType =
  IAM
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PrincipalType where
    parser = takeLowerText >>= \case
        "iam" -> pure IAM
        e -> fromTextError $ "Failure parsing PrincipalType from value: '" <> e
           <> "'. Accepted values: iam"

instance ToText PrincipalType where
    toText = \case
        IAM -> "IAM"

instance Hashable     PrincipalType
instance NFData       PrincipalType
instance ToByteString PrincipalType
instance ToQuery      PrincipalType
instance ToHeader     PrincipalType

instance ToJSON PrincipalType where
    toJSON = toJSONText

instance FromJSON PrincipalType where
    parseJSON = parseJSONText "PrincipalType"

data ProductSource =
  PSAccount
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProductSource where
    parser = takeLowerText >>= \case
        "account" -> pure PSAccount
        e -> fromTextError $ "Failure parsing ProductSource from value: '" <> e
           <> "'. Accepted values: account"

instance ToText ProductSource where
    toText = \case
        PSAccount -> "ACCOUNT"

instance Hashable     ProductSource
instance NFData       ProductSource
instance ToByteString ProductSource
instance ToQuery      ProductSource
instance ToHeader     ProductSource

instance ToJSON ProductSource where
    toJSON = toJSONText

data ProductType
  = CloudFormationTemplate
  | Marketplace
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProductType where
    parser = takeLowerText >>= \case
        "cloud_formation_template" -> pure CloudFormationTemplate
        "marketplace" -> pure Marketplace
        e -> fromTextError $ "Failure parsing ProductType from value: '" <> e
           <> "'. Accepted values: cloud_formation_template, marketplace"

instance ToText ProductType where
    toText = \case
        CloudFormationTemplate -> "CLOUD_FORMATION_TEMPLATE"
        Marketplace -> "MARKETPLACE"

instance Hashable     ProductType
instance NFData       ProductType
instance ToByteString ProductType
instance ToQuery      ProductType
instance ToHeader     ProductType

instance ToJSON ProductType where
    toJSON = toJSONText

instance FromJSON ProductType where
    parseJSON = parseJSONText "ProductType"

data ProductViewFilterBy
  = FullTextSearch
  | Owner
  | ProductType
  | SourceProductId
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProductViewFilterBy where
    parser = takeLowerText >>= \case
        "fulltextsearch" -> pure FullTextSearch
        "owner" -> pure Owner
        "producttype" -> pure ProductType
        "sourceproductid" -> pure SourceProductId
        e -> fromTextError $ "Failure parsing ProductViewFilterBy from value: '" <> e
           <> "'. Accepted values: fulltextsearch, owner, producttype, sourceproductid"

instance ToText ProductViewFilterBy where
    toText = \case
        FullTextSearch -> "FullTextSearch"
        Owner -> "Owner"
        ProductType -> "ProductType"
        SourceProductId -> "SourceProductId"

instance Hashable     ProductViewFilterBy
instance NFData       ProductViewFilterBy
instance ToByteString ProductViewFilterBy
instance ToQuery      ProductViewFilterBy
instance ToHeader     ProductViewFilterBy

instance ToJSON ProductViewFilterBy where
    toJSON = toJSONText

data ProductViewSortBy
  = CreationDate
  | Title
  | VersionCount
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProductViewSortBy where
    parser = takeLowerText >>= \case
        "creationdate" -> pure CreationDate
        "title" -> pure Title
        "versioncount" -> pure VersionCount
        e -> fromTextError $ "Failure parsing ProductViewSortBy from value: '" <> e
           <> "'. Accepted values: creationdate, title, versioncount"

instance ToText ProductViewSortBy where
    toText = \case
        CreationDate -> "CreationDate"
        Title -> "Title"
        VersionCount -> "VersionCount"

instance Hashable     ProductViewSortBy
instance NFData       ProductViewSortBy
instance ToByteString ProductViewSortBy
instance ToQuery      ProductViewSortBy
instance ToHeader     ProductViewSortBy

instance ToJSON ProductViewSortBy where
    toJSON = toJSONText

data ProvisionedProductPlanStatus
  = CreateFailed
  | CreateInProgress
  | CreateSuccess
  | ExecuteFailed
  | ExecuteInProgress
  | ExecuteSuccess
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProvisionedProductPlanStatus where
    parser = takeLowerText >>= \case
        "create_failed" -> pure CreateFailed
        "create_in_progress" -> pure CreateInProgress
        "create_success" -> pure CreateSuccess
        "execute_failed" -> pure ExecuteFailed
        "execute_in_progress" -> pure ExecuteInProgress
        "execute_success" -> pure ExecuteSuccess
        e -> fromTextError $ "Failure parsing ProvisionedProductPlanStatus from value: '" <> e
           <> "'. Accepted values: create_failed, create_in_progress, create_success, execute_failed, execute_in_progress, execute_success"

instance ToText ProvisionedProductPlanStatus where
    toText = \case
        CreateFailed -> "CREATE_FAILED"
        CreateInProgress -> "CREATE_IN_PROGRESS"
        CreateSuccess -> "CREATE_SUCCESS"
        ExecuteFailed -> "EXECUTE_FAILED"
        ExecuteInProgress -> "EXECUTE_IN_PROGRESS"
        ExecuteSuccess -> "EXECUTE_SUCCESS"

instance Hashable     ProvisionedProductPlanStatus
instance NFData       ProvisionedProductPlanStatus
instance ToByteString ProvisionedProductPlanStatus
instance ToQuery      ProvisionedProductPlanStatus
instance ToHeader     ProvisionedProductPlanStatus

instance FromJSON ProvisionedProductPlanStatus where
    parseJSON = parseJSONText "ProvisionedProductPlanStatus"

data ProvisionedProductPlanType =
  Cloudformation
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProvisionedProductPlanType where
    parser = takeLowerText >>= \case
        "cloudformation" -> pure Cloudformation
        e -> fromTextError $ "Failure parsing ProvisionedProductPlanType from value: '" <> e
           <> "'. Accepted values: cloudformation"

instance ToText ProvisionedProductPlanType where
    toText = \case
        Cloudformation -> "CLOUDFORMATION"

instance Hashable     ProvisionedProductPlanType
instance NFData       ProvisionedProductPlanType
instance ToByteString ProvisionedProductPlanType
instance ToQuery      ProvisionedProductPlanType
instance ToHeader     ProvisionedProductPlanType

instance ToJSON ProvisionedProductPlanType where
    toJSON = toJSONText

instance FromJSON ProvisionedProductPlanType where
    parseJSON = parseJSONText "ProvisionedProductPlanType"

data ProvisionedProductStatus
  = PPSAvailable
  | PPSError'
  | PPSPlanInProgress
  | PPSTainted
  | PPSUnderChange
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProvisionedProductStatus where
    parser = takeLowerText >>= \case
        "available" -> pure PPSAvailable
        "error" -> pure PPSError'
        "plan_in_progress" -> pure PPSPlanInProgress
        "tainted" -> pure PPSTainted
        "under_change" -> pure PPSUnderChange
        e -> fromTextError $ "Failure parsing ProvisionedProductStatus from value: '" <> e
           <> "'. Accepted values: available, error, plan_in_progress, tainted, under_change"

instance ToText ProvisionedProductStatus where
    toText = \case
        PPSAvailable -> "AVAILABLE"
        PPSError' -> "ERROR"
        PPSPlanInProgress -> "PLAN_IN_PROGRESS"
        PPSTainted -> "TAINTED"
        PPSUnderChange -> "UNDER_CHANGE"

instance Hashable     ProvisionedProductStatus
instance NFData       ProvisionedProductStatus
instance ToByteString ProvisionedProductStatus
instance ToQuery      ProvisionedProductStatus
instance ToHeader     ProvisionedProductStatus

instance FromJSON ProvisionedProductStatus where
    parseJSON = parseJSONText "ProvisionedProductStatus"

data ProvisionedProductViewFilterBy =
  SearchQuery
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProvisionedProductViewFilterBy where
    parser = takeLowerText >>= \case
        "searchquery" -> pure SearchQuery
        e -> fromTextError $ "Failure parsing ProvisionedProductViewFilterBy from value: '" <> e
           <> "'. Accepted values: searchquery"

instance ToText ProvisionedProductViewFilterBy where
    toText = \case
        SearchQuery -> "SearchQuery"

instance Hashable     ProvisionedProductViewFilterBy
instance NFData       ProvisionedProductViewFilterBy
instance ToByteString ProvisionedProductViewFilterBy
instance ToQuery      ProvisionedProductViewFilterBy
instance ToHeader     ProvisionedProductViewFilterBy

instance ToJSON ProvisionedProductViewFilterBy where
    toJSON = toJSONText

data ProvisioningArtifactPropertyName =
  Id
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProvisioningArtifactPropertyName where
    parser = takeLowerText >>= \case
        "id" -> pure Id
        e -> fromTextError $ "Failure parsing ProvisioningArtifactPropertyName from value: '" <> e
           <> "'. Accepted values: id"

instance ToText ProvisioningArtifactPropertyName where
    toText = \case
        Id -> "Id"

instance Hashable     ProvisioningArtifactPropertyName
instance NFData       ProvisioningArtifactPropertyName
instance ToByteString ProvisioningArtifactPropertyName
instance ToQuery      ProvisioningArtifactPropertyName
instance ToHeader     ProvisioningArtifactPropertyName

instance ToJSON ProvisioningArtifactPropertyName where
    toJSON = toJSONText

data ProvisioningArtifactType
  = PATCloudFormationTemplate
  | PATMarketplaceAMI
  | PATMarketplaceCar
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ProvisioningArtifactType where
    parser = takeLowerText >>= \case
        "cloud_formation_template" -> pure PATCloudFormationTemplate
        "marketplace_ami" -> pure PATMarketplaceAMI
        "marketplace_car" -> pure PATMarketplaceCar
        e -> fromTextError $ "Failure parsing ProvisioningArtifactType from value: '" <> e
           <> "'. Accepted values: cloud_formation_template, marketplace_ami, marketplace_car"

instance ToText ProvisioningArtifactType where
    toText = \case
        PATCloudFormationTemplate -> "CLOUD_FORMATION_TEMPLATE"
        PATMarketplaceAMI -> "MARKETPLACE_AMI"
        PATMarketplaceCar -> "MARKETPLACE_CAR"

instance Hashable     ProvisioningArtifactType
instance NFData       ProvisioningArtifactType
instance ToByteString ProvisioningArtifactType
instance ToQuery      ProvisioningArtifactType
instance ToHeader     ProvisioningArtifactType

instance ToJSON ProvisioningArtifactType where
    toJSON = toJSONText

instance FromJSON ProvisioningArtifactType where
    parseJSON = parseJSONText "ProvisioningArtifactType"

data RecordStatus
  = RSCreated
  | RSFailed
  | RSInProgress
  | RSInProgressInError
  | RSSucceeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RecordStatus where
    parser = takeLowerText >>= \case
        "created" -> pure RSCreated
        "failed" -> pure RSFailed
        "in_progress" -> pure RSInProgress
        "in_progress_in_error" -> pure RSInProgressInError
        "succeeded" -> pure RSSucceeded
        e -> fromTextError $ "Failure parsing RecordStatus from value: '" <> e
           <> "'. Accepted values: created, failed, in_progress, in_progress_in_error, succeeded"

instance ToText RecordStatus where
    toText = \case
        RSCreated -> "CREATED"
        RSFailed -> "FAILED"
        RSInProgress -> "IN_PROGRESS"
        RSInProgressInError -> "IN_PROGRESS_IN_ERROR"
        RSSucceeded -> "SUCCEEDED"

instance Hashable     RecordStatus
instance NFData       RecordStatus
instance ToByteString RecordStatus
instance ToQuery      RecordStatus
instance ToHeader     RecordStatus

instance FromJSON RecordStatus where
    parseJSON = parseJSONText "RecordStatus"

data Replacement
  = Conditional
  | False'
  | True'
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Replacement where
    parser = takeLowerText >>= \case
        "conditional" -> pure Conditional
        "false" -> pure False'
        "true" -> pure True'
        e -> fromTextError $ "Failure parsing Replacement from value: '" <> e
           <> "'. Accepted values: conditional, false, true"

instance ToText Replacement where
    toText = \case
        Conditional -> "CONDITIONAL"
        False' -> "FALSE"
        True' -> "TRUE"

instance Hashable     Replacement
instance NFData       Replacement
instance ToByteString Replacement
instance ToQuery      Replacement
instance ToHeader     Replacement

instance FromJSON Replacement where
    parseJSON = parseJSONText "Replacement"

data RequestStatus
  = Available
  | Creating
  | Failed
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RequestStatus where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "creating" -> pure Creating
        "failed" -> pure Failed
        e -> fromTextError $ "Failure parsing RequestStatus from value: '" <> e
           <> "'. Accepted values: available, creating, failed"

instance ToText RequestStatus where
    toText = \case
        Available -> "AVAILABLE"
        Creating -> "CREATING"
        Failed -> "FAILED"

instance Hashable     RequestStatus
instance NFData       RequestStatus
instance ToByteString RequestStatus
instance ToQuery      RequestStatus
instance ToHeader     RequestStatus

instance FromJSON RequestStatus where
    parseJSON = parseJSONText "RequestStatus"

data RequiresRecreation
  = Always
  | Conditionally
  | Never
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RequiresRecreation where
    parser = takeLowerText >>= \case
        "always" -> pure Always
        "conditionally" -> pure Conditionally
        "never" -> pure Never
        e -> fromTextError $ "Failure parsing RequiresRecreation from value: '" <> e
           <> "'. Accepted values: always, conditionally, never"

instance ToText RequiresRecreation where
    toText = \case
        Always -> "ALWAYS"
        Conditionally -> "CONDITIONALLY"
        Never -> "NEVER"

instance Hashable     RequiresRecreation
instance NFData       RequiresRecreation
instance ToByteString RequiresRecreation
instance ToQuery      RequiresRecreation
instance ToHeader     RequiresRecreation

instance FromJSON RequiresRecreation where
    parseJSON = parseJSONText "RequiresRecreation"

data ResourceAttribute
  = Creationpolicy
  | Deletionpolicy
  | Metadata
  | Properties
  | Tags
  | Updatepolicy
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceAttribute where
    parser = takeLowerText >>= \case
        "creationpolicy" -> pure Creationpolicy
        "deletionpolicy" -> pure Deletionpolicy
        "metadata" -> pure Metadata
        "properties" -> pure Properties
        "tags" -> pure Tags
        "updatepolicy" -> pure Updatepolicy
        e -> fromTextError $ "Failure parsing ResourceAttribute from value: '" <> e
           <> "'. Accepted values: creationpolicy, deletionpolicy, metadata, properties, tags, updatepolicy"

instance ToText ResourceAttribute where
    toText = \case
        Creationpolicy -> "CREATIONPOLICY"
        Deletionpolicy -> "DELETIONPOLICY"
        Metadata -> "METADATA"
        Properties -> "PROPERTIES"
        Tags -> "TAGS"
        Updatepolicy -> "UPDATEPOLICY"

instance Hashable     ResourceAttribute
instance NFData       ResourceAttribute
instance ToByteString ResourceAttribute
instance ToQuery      ResourceAttribute
instance ToHeader     ResourceAttribute

instance FromJSON ResourceAttribute where
    parseJSON = parseJSONText "ResourceAttribute"

data ServiceActionAssociationErrorCode
  = DuplicateResource
  | InternalFailure
  | LimitExceeded
  | ResourceNotFound
  | Throttling
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ServiceActionAssociationErrorCode where
    parser = takeLowerText >>= \case
        "duplicate_resource" -> pure DuplicateResource
        "internal_failure" -> pure InternalFailure
        "limit_exceeded" -> pure LimitExceeded
        "resource_not_found" -> pure ResourceNotFound
        "throttling" -> pure Throttling
        e -> fromTextError $ "Failure parsing ServiceActionAssociationErrorCode from value: '" <> e
           <> "'. Accepted values: duplicate_resource, internal_failure, limit_exceeded, resource_not_found, throttling"

instance ToText ServiceActionAssociationErrorCode where
    toText = \case
        DuplicateResource -> "DUPLICATE_RESOURCE"
        InternalFailure -> "INTERNAL_FAILURE"
        LimitExceeded -> "LIMIT_EXCEEDED"
        ResourceNotFound -> "RESOURCE_NOT_FOUND"
        Throttling -> "THROTTLING"

instance Hashable     ServiceActionAssociationErrorCode
instance NFData       ServiceActionAssociationErrorCode
instance ToByteString ServiceActionAssociationErrorCode
instance ToQuery      ServiceActionAssociationErrorCode
instance ToHeader     ServiceActionAssociationErrorCode

instance FromJSON ServiceActionAssociationErrorCode where
    parseJSON = parseJSONText "ServiceActionAssociationErrorCode"

data ServiceActionDefinitionKey
  = AssumeRole
  | Name
  | Parameters
  | Version
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ServiceActionDefinitionKey where
    parser = takeLowerText >>= \case
        "assumerole" -> pure AssumeRole
        "name" -> pure Name
        "parameters" -> pure Parameters
        "version" -> pure Version
        e -> fromTextError $ "Failure parsing ServiceActionDefinitionKey from value: '" <> e
           <> "'. Accepted values: assumerole, name, parameters, version"

instance ToText ServiceActionDefinitionKey where
    toText = \case
        AssumeRole -> "AssumeRole"
        Name -> "Name"
        Parameters -> "Parameters"
        Version -> "Version"

instance Hashable     ServiceActionDefinitionKey
instance NFData       ServiceActionDefinitionKey
instance ToByteString ServiceActionDefinitionKey
instance ToQuery      ServiceActionDefinitionKey
instance ToHeader     ServiceActionDefinitionKey

instance ToJSON ServiceActionDefinitionKey where
    toJSON = toJSONText

instance FromJSON ServiceActionDefinitionKey where
    parseJSON = parseJSONText "ServiceActionDefinitionKey"

data ServiceActionDefinitionType =
  SsmAutomation
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ServiceActionDefinitionType where
    parser = takeLowerText >>= \case
        "ssm_automation" -> pure SsmAutomation
        e -> fromTextError $ "Failure parsing ServiceActionDefinitionType from value: '" <> e
           <> "'. Accepted values: ssm_automation"

instance ToText ServiceActionDefinitionType where
    toText = \case
        SsmAutomation -> "SSM_AUTOMATION"

instance Hashable     ServiceActionDefinitionType
instance NFData       ServiceActionDefinitionType
instance ToByteString ServiceActionDefinitionType
instance ToQuery      ServiceActionDefinitionType
instance ToHeader     ServiceActionDefinitionType

instance ToJSON ServiceActionDefinitionType where
    toJSON = toJSONText

instance FromJSON ServiceActionDefinitionType where
    parseJSON = parseJSONText "ServiceActionDefinitionType"

data ShareStatus
  = Completed
  | CompletedWithErrors
  | Error'
  | InProgress
  | NotStarted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ShareStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure Completed
        "completed_with_errors" -> pure CompletedWithErrors
        "error" -> pure Error'
        "in_progress" -> pure InProgress
        "not_started" -> pure NotStarted
        e -> fromTextError $ "Failure parsing ShareStatus from value: '" <> e
           <> "'. Accepted values: completed, completed_with_errors, error, in_progress, not_started"

instance ToText ShareStatus where
    toText = \case
        Completed -> "COMPLETED"
        CompletedWithErrors -> "COMPLETED_WITH_ERRORS"
        Error' -> "ERROR"
        InProgress -> "IN_PROGRESS"
        NotStarted -> "NOT_STARTED"

instance Hashable     ShareStatus
instance NFData       ShareStatus
instance ToByteString ShareStatus
instance ToQuery      ShareStatus
instance ToHeader     ShareStatus

instance FromJSON ShareStatus where
    parseJSON = parseJSONText "ShareStatus"

data SortOrder
  = Ascending
  | Descending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SortOrder where
    parser = takeLowerText >>= \case
        "ascending" -> pure Ascending
        "descending" -> pure Descending
        e -> fromTextError $ "Failure parsing SortOrder from value: '" <> e
           <> "'. Accepted values: ascending, descending"

instance ToText SortOrder where
    toText = \case
        Ascending -> "ASCENDING"
        Descending -> "DESCENDING"

instance Hashable     SortOrder
instance NFData       SortOrder
instance ToByteString SortOrder
instance ToQuery      SortOrder
instance ToHeader     SortOrder

instance ToJSON SortOrder where
    toJSON = toJSONText

data StackSetOperationType
  = Create
  | Delete
  | Update
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackSetOperationType where
    parser = takeLowerText >>= \case
        "create" -> pure Create
        "delete" -> pure Delete
        "update" -> pure Update
        e -> fromTextError $ "Failure parsing StackSetOperationType from value: '" <> e
           <> "'. Accepted values: create, delete, update"

instance ToText StackSetOperationType where
    toText = \case
        Create -> "CREATE"
        Delete -> "DELETE"
        Update -> "UPDATE"

instance Hashable     StackSetOperationType
instance NFData       StackSetOperationType
instance ToByteString StackSetOperationType
instance ToQuery      StackSetOperationType
instance ToHeader     StackSetOperationType

instance ToJSON StackSetOperationType where
    toJSON = toJSONText
