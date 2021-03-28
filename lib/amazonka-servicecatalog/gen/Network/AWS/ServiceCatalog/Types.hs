-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _InvalidParametersException
    , _DuplicateResourceException
    , _OperationNotSupportedException
    , _TagOptionNotMigratedException
    , _ResourceNotFoundException
    , _InvalidStateException
    , _LimitExceededException
    , _ResourceInUseException

    -- * ResourceChange
    , ResourceChange (..)
    , mkResourceChange
    , rcAction
    , rcDetails
    , rcLogicalResourceId
    , rcPhysicalResourceId
    , rcReplacement
    , rcResourceType
    , rcScope

    -- * OutputValue
    , OutputValue (..)

    -- * CloudWatchDashboard
    , CloudWatchDashboard (..)
    , mkCloudWatchDashboard
    , cwdName

    -- * ExecutionParameterKey
    , ExecutionParameterKey (..)

    -- * IdempotencyToken
    , IdempotencyToken (..)

    -- * RequestStatus
    , RequestStatus (..)

    -- * LogicalResourceId
    , LogicalResourceId (..)

    -- * ServiceActionName
    , ServiceActionName (..)

    -- * RecordDetail
    , RecordDetail (..)
    , mkRecordDetail
    , rdCreatedTime
    , rdLaunchRoleArn
    , rdPathId
    , rdProductId
    , rdProvisionedProductId
    , rdProvisionedProductName
    , rdProvisionedProductType
    , rdProvisioningArtifactId
    , rdRecordErrors
    , rdRecordId
    , rdRecordTags
    , rdRecordType
    , rdStatus
    , rdUpdatedTime

    -- * LaunchPathSummary
    , LaunchPathSummary (..)
    , mkLaunchPathSummary
    , lpsConstraintSummaries
    , lpsId
    , lpsName
    , lpsTags

    -- * ConstraintDetail
    , ConstraintDetail (..)
    , mkConstraintDetail
    , cdConstraintId
    , cdDescription
    , cdOwner
    , cdPortfolioId
    , cdProductId
    , cdType

    -- * TagOptionId
    , TagOptionId (..)

    -- * ProvisionedProductNameOrArn
    , ProvisionedProductNameOrArn (..)

    -- * ProvisionedProductName
    , ProvisionedProductName (..)

    -- * RecordTag
    , RecordTag (..)
    , mkRecordTag
    , rtKey
    , rtValue

    -- * ShareDetails
    , ShareDetails (..)
    , mkShareDetails
    , sdShareErrors
    , sdSuccessfulShares

    -- * ConstraintParameters
    , ConstraintParameters (..)

    -- * ProductViewOwner
    , ProductViewOwner (..)

    -- * ServiceActionAssociationErrorCode
    , ServiceActionAssociationErrorCode (..)

    -- * ProvisionedProductAttribute
    , ProvisionedProductAttribute (..)
    , mkProvisionedProductAttribute
    , ppaArn
    , ppaCreatedTime
    , ppaId
    , ppaIdempotencyToken
    , ppaLastProvisioningRecordId
    , ppaLastRecordId
    , ppaLastSuccessfulProvisioningRecordId
    , ppaName
    , ppaPhysicalId
    , ppaProductId
    , ppaProductName
    , ppaProvisioningArtifactId
    , ppaProvisioningArtifactName
    , ppaStatus
    , ppaStatusMessage
    , ppaTags
    , ppaType
    , ppaUserArn
    , ppaUserArnSession

    -- * ProvisionedProductPlanName
    , ProvisionedProductPlanName (..)

    -- * ServiceActionSummary
    , ServiceActionSummary (..)
    , mkServiceActionSummary
    , sasDefinitionType
    , sasDescription
    , sasId
    , sasName

    -- * ProvisioningArtifactType
    , ProvisioningArtifactType (..)

    -- * ProvisioningArtifactPreferences
    , ProvisioningArtifactPreferences (..)
    , mkProvisioningArtifactPreferences
    , papStackSetAccounts
    , papStackSetRegions

    -- * PortfolioDisplayName
    , PortfolioDisplayName (..)

    -- * CausingEntity
    , CausingEntity (..)

    -- * AttributeValue
    , AttributeValue (..)

    -- * ResourceId
    , ResourceId (..)

    -- * PortfolioShareType
    , PortfolioShareType (..)

    -- * ProvisioningArtifactPropertyName
    , ProvisioningArtifactPropertyName (..)

    -- * ServiceActionDetail
    , ServiceActionDetail (..)
    , mkServiceActionDetail
    , sadDefinition
    , sadServiceActionSummary

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * TagOptionValue
    , TagOptionValue (..)

    -- * CopyProductStatus
    , CopyProductStatus (..)

    -- * PhysicalResourceId
    , PhysicalResourceId (..)

    -- * ProductViewSummary
    , ProductViewSummary (..)
    , mkProductViewSummary
    , pvsDistributor
    , pvsHasDefaultPath
    , pvsId
    , pvsName
    , pvsOwner
    , pvsProductId
    , pvsShortDescription
    , pvsSupportDescription
    , pvsSupportEmail
    , pvsSupportUrl
    , pvsType

    -- * ProvisionedProductDetail
    , ProvisionedProductDetail (..)
    , mkProvisionedProductDetail
    , ppdArn
    , ppdCreatedTime
    , ppdId
    , ppdIdempotencyToken
    , ppdLastProvisioningRecordId
    , ppdLastRecordId
    , ppdLastSuccessfulProvisioningRecordId
    , ppdLaunchRoleArn
    , ppdName
    , ppdProductId
    , ppdProvisioningArtifactId
    , ppdStatus
    , ppdStatusMessage
    , ppdType

    -- * ProvisioningArtifactInfoValue
    , ProvisioningArtifactInfoValue (..)

    -- * RecordOutput
    , RecordOutput (..)
    , mkRecordOutput
    , roDescription
    , roOutputKey
    , roOutputValue

    -- * ProvisioningArtifactView
    , ProvisioningArtifactView (..)
    , mkProvisioningArtifactView
    , pavProductViewSummary
    , pavProvisioningArtifact

    -- * ResourceType
    , ResourceType (..)

    -- * ProvisionedProductPlanDetails
    , ProvisionedProductPlanDetails (..)
    , mkProvisionedProductPlanDetails
    , pppdCreatedTime
    , pppdNotificationArns
    , pppdPathId
    , pppdPlanId
    , pppdPlanName
    , pppdPlanType
    , pppdProductId
    , pppdProvisionProductId
    , pppdProvisionProductName
    , pppdProvisioningArtifactId
    , pppdProvisioningParameters
    , pppdStatus
    , pppdStatusMessage
    , pppdTags
    , pppdUpdatedTime

    -- * PrincipalType
    , PrincipalType (..)

    -- * ParameterValue
    , ParameterValue (..)

    -- * ProductViewAggregationType
    , ProductViewAggregationType (..)

    -- * AccessLevelFilterKey
    , AccessLevelFilterKey (..)

    -- * ServiceActionDescription
    , ServiceActionDescription (..)

    -- * PortfolioName
    , PortfolioName (..)

    -- * TagOptionSummary
    , TagOptionSummary (..)
    , mkTagOptionSummary
    , tosKey
    , tosValues

    -- * RecordType
    , RecordType (..)

    -- * ConstraintType
    , ConstraintType (..)

    -- * RecordError
    , RecordError (..)
    , mkRecordError
    , reCode
    , reDescription

    -- * ChangeAction
    , ChangeAction (..)

    -- * AccessStatus
    , AccessStatus (..)

    -- * InstructionType
    , InstructionType (..)

    -- * TagOptionDetail
    , TagOptionDetail (..)
    , mkTagOptionDetail
    , todActive
    , todId
    , todKey
    , todValue

    -- * OrganizationNodeValue
    , OrganizationNodeValue (..)

    -- * ProvisioningArtifactName
    , ProvisioningArtifactName (..)

    -- * ProvisionedProductPlanType
    , ProvisionedProductPlanType (..)

    -- * RequiresRecreation
    , RequiresRecreation (..)

    -- * PropertyKey
    , PropertyKey (..)

    -- * ProvisioningArtifactOutput
    , ProvisioningArtifactOutput (..)
    , mkProvisioningArtifactOutput
    , paoDescription
    , paoKey

    -- * LaunchPath
    , LaunchPath (..)
    , mkLaunchPath
    , lpId
    , lpName

    -- * Error
    , Error (..)

    -- * ProvisioningParameter
    , ProvisioningParameter (..)
    , mkProvisioningParameter
    , ppKey
    , ppValue

    -- * ExecutionParameter
    , ExecutionParameter (..)
    , mkExecutionParameter
    , epDefaultValues
    , epName
    , epType

    -- * OrganizationNodeType
    , OrganizationNodeType (..)

    -- * ProvisionedProductType
    , ProvisionedProductType (..)

    -- * PortfolioDetail
    , PortfolioDetail (..)
    , mkPortfolioDetail
    , pdARN
    , pdCreatedTime
    , pdDescription
    , pdDisplayName
    , pdId
    , pdProviderName

    -- * ConstraintDescription
    , ConstraintDescription (..)

    -- * ProvisioningArtifactSummary
    , ProvisioningArtifactSummary (..)
    , mkProvisioningArtifactSummary
    , pasCreatedTime
    , pasDescription
    , pasId
    , pasName
    , pasProvisioningArtifactMetadata

    -- * ProductType
    , ProductType (..)

    -- * ResourceDetail
    , ResourceDetail (..)
    , mkResourceDetail
    , rARN
    , rCreatedTime
    , rDescription
    , rId
    , rName

    -- * ProvisioningArtifactParameter
    , ProvisioningArtifactParameter (..)
    , mkProvisioningArtifactParameter
    , papDefaultValue
    , papDescription
    , papIsNoEcho
    , papParameterConstraints
    , papParameterKey
    , papParameterType

    -- * ResourceDetailId
    , ResourceDetailId (..)

    -- * ParameterKey
    , ParameterKey (..)

    -- * ResourceAttribute
    , ResourceAttribute (..)

    -- * CopyOption
    , CopyOption (..)

    -- * TagOptionKey
    , TagOptionKey (..)

    -- * EvaluationType
    , EvaluationType (..)

    -- * ProvisioningArtifactInfoKey
    , ProvisioningArtifactInfoKey (..)

    -- * ParameterType
    , ParameterType (..)

    -- * ProvisioningArtifactDescription
    , ProvisioningArtifactDescription (..)

    -- * UpdateProvisioningParameter
    , UpdateProvisioningParameter (..)
    , mkUpdateProvisioningParameter
    , uppKey
    , uppUsePreviousValue
    , uppValue

    -- * ConstraintSummary
    , ConstraintSummary (..)
    , mkConstraintSummary
    , csDescription
    , csType

    -- * ServiceActionAssociationErrorMessage
    , ServiceActionAssociationErrorMessage (..)

    -- * SearchFilterKey
    , SearchFilterKey (..)

    -- * ServiceActionDefinitionKey
    , ServiceActionDefinitionKey (..)

    -- * ShareStatus
    , ShareStatus (..)

    -- * FailedServiceActionAssociation
    , FailedServiceActionAssociation (..)
    , mkFailedServiceActionAssociation
    , fsaaErrorCode
    , fsaaErrorMessage
    , fsaaProductId
    , fsaaProvisioningArtifactId
    , fsaaServiceActionId

    -- * AccountId
    , AccountId (..)

    -- * BudgetDetail
    , BudgetDetail (..)
    , mkBudgetDetail
    , bdBudgetName

    -- * PropertyValue
    , PropertyValue (..)

    -- * ProvisionedProductPlanSummary
    , ProvisionedProductPlanSummary (..)
    , mkProvisionedProductPlanSummary
    , pppsPlanId
    , pppsPlanName
    , pppsPlanType
    , pppsProvisionProductId
    , pppsProvisionProductName
    , pppsProvisioningArtifactId

    -- * SupportUrl
    , SupportUrl (..)

    -- * ProvisionedProductPlanStatus
    , ProvisionedProductPlanStatus (..)

    -- * ProvisionedProductStatusMessage
    , ProvisionedProductStatusMessage (..)

    -- * UserArn
    , UserArn (..)

    -- * SortOrder
    , SortOrder (..)

    -- * PortfolioDescription
    , PortfolioDescription (..)

    -- * ProvisioningArtifact
    , ProvisioningArtifact (..)
    , mkProvisioningArtifact
    , paCreatedTime
    , paDescription
    , paGuidance
    , paId
    , paName

    -- * ParameterConstraints
    , ParameterConstraints (..)
    , mkParameterConstraints
    , pcAllowedValues

    -- * PrincipalARN
    , PrincipalARN (..)

    -- * ResourceARN
    , ResourceARN (..)

    -- * ProductViewSortBy
    , ProductViewSortBy (..)

    -- * StackInstance
    , StackInstance (..)
    , mkStackInstance
    , siAccount
    , siRegion
    , siStackInstanceStatus

    -- * ProductViewDetail
    , ProductViewDetail (..)
    , mkProductViewDetail
    , pvdCreatedTime
    , pvdProductARN
    , pvdProductViewSummary
    , pvdStatus

    -- * StatusMessage
    , StatusMessage (..)

    -- * ProvisionedProductStatus
    , ProvisionedProductStatus (..)

    -- * Principal
    , Principal (..)
    , mkPrincipal
    , pPrincipalARN
    , pPrincipalType

    -- * AcceptLanguage
    , AcceptLanguage (..)

    -- * AccessLevelFilter
    , AccessLevelFilter (..)
    , mkAccessLevelFilter
    , alfKey
    , alfValue

    -- * ServiceActionAssociation
    , ServiceActionAssociation (..)
    , mkServiceActionAssociation
    , saaServiceActionId
    , saaProductId
    , saaProvisioningArtifactId

    -- * BudgetName
    , BudgetName (..)

    -- * UserArnSession
    , UserArnSession (..)

    -- * StatusDetail
    , StatusDetail (..)

    -- * Id
    , Id (..)

    -- * ProvisionedProductViewFilterValue
    , ProvisionedProductViewFilterValue (..)

    -- * ProvisionedProductViewFilterBy
    , ProvisionedProductViewFilterBy (..)

    -- * SearchFilterValue
    , SearchFilterValue (..)

    -- * ResourceChangeDetail
    , ResourceChangeDetail (..)
    , mkResourceChangeDetail
    , rcdCausingEntity
    , rcdEvaluation
    , rcdTarget

    -- * ProductViewFilterValue
    , ProductViewFilterValue (..)

    -- * UsageInstruction
    , UsageInstruction (..)
    , mkUsageInstruction
    , uiType
    , uiValue

    -- * ShareError
    , ShareError (..)
    , mkShareError
    , seAccounts
    , seError
    , seMessage

    -- * ServiceActionDefinitionValue
    , ServiceActionDefinitionValue (..)

    -- * TagKey
    , TagKey (..)

    -- * ProductViewFilterBy
    , ProductViewFilterBy (..)

    -- * DefaultValue
    , DefaultValue (..)

    -- * Region
    , Region (..)

    -- * NotificationArn
    , NotificationArn (..)

    -- * PropertyName
    , PropertyName (..)

    -- * ExecutionParameterValue
    , ExecutionParameterValue (..)

    -- * OutputKey
    , OutputKey (..)

    -- * UpdateProvisioningPreferences
    , UpdateProvisioningPreferences (..)
    , mkUpdateProvisioningPreferences
    , uppStackSetAccounts
    , uppStackSetFailureToleranceCount
    , uppStackSetFailureTolerancePercentage
    , uppStackSetMaxConcurrencyCount
    , uppStackSetMaxConcurrencyPercentage
    , uppStackSetOperationType
    , uppStackSetRegions

    -- * PageToken
    , PageToken (..)

    -- * PhysicalId
    , PhysicalId (..)

    -- * ListTagOptionsFilters
    , ListTagOptionsFilters (..)
    , mkListTagOptionsFilters
    , ltofActive
    , ltofKey
    , ltofValue

    -- * SupportEmail
    , SupportEmail (..)

    -- * ProvisioningArtifactPropertyValue
    , ProvisioningArtifactPropertyValue (..)

    -- * Message
    , Message (..)

    -- * StackInstanceStatus
    , StackInstanceStatus (..)

    -- * OrganizationNode
    , OrganizationNode (..)
    , mkOrganizationNode
    , onType
    , onValue

    -- * AllowedValue
    , AllowedValue (..)

    -- * ListRecordHistorySearchFilter
    , ListRecordHistorySearchFilter (..)
    , mkListRecordHistorySearchFilter
    , lrhsfKey
    , lrhsfValue

    -- * Replacement
    , Replacement (..)

    -- * ProvisionedProductId
    , ProvisionedProductId (..)

    -- * Description
    , Description (..)

    -- * ProductSource
    , ProductSource (..)

    -- * ProviderName
    , ProviderName (..)

    -- * ProvisioningArtifactProperties
    , ProvisioningArtifactProperties (..)
    , mkProvisioningArtifactProperties
    , pInfo
    , pDescription
    , pDisableTemplateValidation
    , pName
    , pType

    -- * ProvisioningArtifactDetail
    , ProvisioningArtifactDetail (..)
    , mkProvisioningArtifactDetail
    , padActive
    , padCreatedTime
    , padDescription
    , padGuidance
    , padId
    , padName
    , padType

    -- * ServiceActionDefinitionType
    , ServiceActionDefinitionType (..)

    -- * ResourceTargetDefinition
    , ResourceTargetDefinition (..)
    , mkResourceTargetDefinition
    , rtdAttribute
    , rtdName
    , rtdRequiresRecreation

    -- * RecordStatus
    , RecordStatus (..)

    -- * ProductViewAggregationValue
    , ProductViewAggregationValue (..)
    , mkProductViewAggregationValue
    , pvavApproximateCount
    , pvavValue

    -- * SupportDescription
    , SupportDescription (..)

    -- * ProvisioningArtifactGuidance
    , ProvisioningArtifactGuidance (..)

    -- * ProvisioningPreferences
    , ProvisioningPreferences (..)
    , mkProvisioningPreferences
    , ppStackSetAccounts
    , ppStackSetFailureToleranceCount
    , ppStackSetFailureTolerancePercentage
    , ppStackSetMaxConcurrencyCount
    , ppStackSetMaxConcurrencyPercentage
    , ppStackSetRegions

    -- * StackSetOperationType
    , StackSetOperationType (..)

    -- * NextPageToken
    , NextPageToken (..)

    -- * Name
    , Name (..)

    -- * Distributor
    , Distributor (..)

    -- * Owner
    , Owner (..)

    -- * PortfolioId
    , PortfolioId (..)

    -- * RecordId
    , RecordId (..)

    -- * PathId
    , PathId (..)

    -- * PathName
    , PathName (..)

    -- * ProductId
    , ProductId (..)

    -- * ProductName
    , ProductName (..)

    -- * ProvisioningArtifactId
    , ProvisioningArtifactId (..)

    -- * LaunchRoleArn
    , LaunchRoleArn (..)

    -- * ConstraintId
    , ConstraintId (..)

    -- * Type
    , Type (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * PlanId
    , PlanId (..)

    -- * ProvisionProductId
    , ProvisionProductId (..)

    -- * LastProvisioningRecordId
    , LastProvisioningRecordId (..)

    -- * LastRecordId
    , LastRecordId (..)

    -- * LastSuccessfulProvisioningRecordId
    , LastSuccessfulProvisioningRecordId (..)

    -- * ShortDescription
    , ShortDescription (..)

    -- * OrganizationParentId
    , OrganizationParentId (..)

    -- * Code
    , Code (..)

    -- * ServiceActionId
    , ServiceActionId (..)

    -- * SortBy
    , SortBy (..)

    -- * ARN
    , ARN (..)

    -- * CopyProductToken
    , CopyProductToken (..)

    -- * PortfolioShareToken
    , PortfolioShareToken (..)

    -- * SourceProductArn
    , SourceProductArn (..)

    -- * TargetProductName
    , TargetProductName (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.ServiceCatalog.Types.ResourceChange
  
import Network.AWS.ServiceCatalog.Types.OutputValue
  
import Network.AWS.ServiceCatalog.Types.CloudWatchDashboard
  
import Network.AWS.ServiceCatalog.Types.ExecutionParameterKey
  
import Network.AWS.ServiceCatalog.Types.IdempotencyToken
  
import Network.AWS.ServiceCatalog.Types.RequestStatus
  
import Network.AWS.ServiceCatalog.Types.LogicalResourceId
  
import Network.AWS.ServiceCatalog.Types.ServiceActionName
  
import Network.AWS.ServiceCatalog.Types.RecordDetail
  
import Network.AWS.ServiceCatalog.Types.LaunchPathSummary
  
import Network.AWS.ServiceCatalog.Types.ConstraintDetail
  
import Network.AWS.ServiceCatalog.Types.TagOptionId
  
import Network.AWS.ServiceCatalog.Types.ProvisionedProductNameOrArn
  
  
import Network.AWS.ServiceCatalog.Types.ProvisionedProductName
  
import Network.AWS.ServiceCatalog.Types.RecordTag
  
import Network.AWS.ServiceCatalog.Types.ShareDetails
  
import Network.AWS.ServiceCatalog.Types.ConstraintParameters
  
import Network.AWS.ServiceCatalog.Types.ProductViewOwner
  
import Network.AWS.ServiceCatalog.Types.ServiceActionAssociationErrorCode
  
import Network.AWS.ServiceCatalog.Types.ProvisionedProductAttribute
  
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanName
  
import Network.AWS.ServiceCatalog.Types.ServiceActionSummary
  
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactType
  
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactPreferences
  
import Network.AWS.ServiceCatalog.Types.PortfolioDisplayName
  
import Network.AWS.ServiceCatalog.Types.CausingEntity
  
import Network.AWS.ServiceCatalog.Types.AttributeValue
  
import Network.AWS.ServiceCatalog.Types.ResourceId
  
import Network.AWS.ServiceCatalog.Types.PortfolioShareType
  
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactPropertyName
  
import Network.AWS.ServiceCatalog.Types.ServiceActionDetail
  
import Network.AWS.ServiceCatalog.Types.Tag
  
import Network.AWS.ServiceCatalog.Types.TagOptionValue
  
import Network.AWS.ServiceCatalog.Types.CopyProductStatus
  
import Network.AWS.ServiceCatalog.Types.PhysicalResourceId
  
import Network.AWS.ServiceCatalog.Types.ProductViewSummary
  
  
import Network.AWS.ServiceCatalog.Types.ProvisionedProductDetail
  
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactInfoValue
  
import Network.AWS.ServiceCatalog.Types.RecordOutput
  
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactView
  
import Network.AWS.ServiceCatalog.Types.ResourceType
  
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanDetails
  
import Network.AWS.ServiceCatalog.Types.PrincipalType
  
import Network.AWS.ServiceCatalog.Types.ParameterValue
  
import Network.AWS.ServiceCatalog.Types.ProductViewAggregationType
  
import Network.AWS.ServiceCatalog.Types.AccessLevelFilterKey
  
import Network.AWS.ServiceCatalog.Types.ServiceActionDescription
  
import Network.AWS.ServiceCatalog.Types.PortfolioName
  
import Network.AWS.ServiceCatalog.Types.TagOptionSummary
  
import Network.AWS.ServiceCatalog.Types.RecordType
  
import Network.AWS.ServiceCatalog.Types.ConstraintType
  
import Network.AWS.ServiceCatalog.Types.RecordError
  
import Network.AWS.ServiceCatalog.Types.ChangeAction
  
import Network.AWS.ServiceCatalog.Types.AccessStatus
  
import Network.AWS.ServiceCatalog.Types.InstructionType
  
import Network.AWS.ServiceCatalog.Types.TagOptionDetail
  
import Network.AWS.ServiceCatalog.Types.OrganizationNodeValue
  
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactName
  
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanType
  
import Network.AWS.ServiceCatalog.Types.RequiresRecreation
  
import Network.AWS.ServiceCatalog.Types.PropertyKey
  
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactOutput
  
import Network.AWS.ServiceCatalog.Types.LaunchPath
  
import Network.AWS.ServiceCatalog.Types.Error
  
import Network.AWS.ServiceCatalog.Types.ProvisioningParameter
  
import Network.AWS.ServiceCatalog.Types.ExecutionParameter
  
import Network.AWS.ServiceCatalog.Types.OrganizationNodeType
  
import Network.AWS.ServiceCatalog.Types.ProvisionedProductType
  
import Network.AWS.ServiceCatalog.Types.PortfolioDetail
  
import Network.AWS.ServiceCatalog.Types.ConstraintDescription
  
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactSummary
  
import Network.AWS.ServiceCatalog.Types.ProductType
  
import Network.AWS.ServiceCatalog.Types.ResourceDetail
  
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactParameter
  
import Network.AWS.ServiceCatalog.Types.ResourceDetailId
  
import Network.AWS.ServiceCatalog.Types.ParameterKey
  
import Network.AWS.ServiceCatalog.Types.ResourceAttribute
  
import Network.AWS.ServiceCatalog.Types.CopyOption
  
import Network.AWS.ServiceCatalog.Types.TagOptionKey
  
import Network.AWS.ServiceCatalog.Types.EvaluationType
  
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactInfoKey
  
import Network.AWS.ServiceCatalog.Types.ParameterType
  
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactDescription
  
import Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter
  
import Network.AWS.ServiceCatalog.Types.ConstraintSummary
  
import Network.AWS.ServiceCatalog.Types.ServiceActionAssociationErrorMessage
  
import Network.AWS.ServiceCatalog.Types.SearchFilterKey
  
import Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionKey
  
  
import Network.AWS.ServiceCatalog.Types.ShareStatus
  
import Network.AWS.ServiceCatalog.Types.FailedServiceActionAssociation
  
import Network.AWS.ServiceCatalog.Types.AccountId
  
import Network.AWS.ServiceCatalog.Types.BudgetDetail
  
import Network.AWS.ServiceCatalog.Types.PropertyValue
  
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanSummary
  
import Network.AWS.ServiceCatalog.Types.SupportUrl
  
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanStatus
  
import Network.AWS.ServiceCatalog.Types.ProvisionedProductStatusMessage
  
import Network.AWS.ServiceCatalog.Types.UserArn
  
import Network.AWS.ServiceCatalog.Types.SortOrder
  
import Network.AWS.ServiceCatalog.Types.PortfolioDescription
  
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifact
  
import Network.AWS.ServiceCatalog.Types.ParameterConstraints
  
import Network.AWS.ServiceCatalog.Types.PrincipalARN
  
import Network.AWS.ServiceCatalog.Types.ResourceARN
  
import Network.AWS.ServiceCatalog.Types.ProductViewSortBy
  
import Network.AWS.ServiceCatalog.Types.StackInstance
  
import Network.AWS.ServiceCatalog.Types.ProductViewDetail
  
import Network.AWS.ServiceCatalog.Types.StatusMessage
  
import Network.AWS.ServiceCatalog.Types.ProvisionedProductStatus
  
import Network.AWS.ServiceCatalog.Types.Principal
  
import Network.AWS.ServiceCatalog.Types.AcceptLanguage
  
import Network.AWS.ServiceCatalog.Types.AccessLevelFilter
  
import Network.AWS.ServiceCatalog.Types.ServiceActionAssociation
  
import Network.AWS.ServiceCatalog.Types.BudgetName
  
import Network.AWS.ServiceCatalog.Types.UserArnSession
  
import Network.AWS.ServiceCatalog.Types.StatusDetail
  
import Network.AWS.ServiceCatalog.Types.Id
  
import Network.AWS.ServiceCatalog.Types.ProvisionedProductViewFilterValue
  
import Network.AWS.ServiceCatalog.Types.ProvisionedProductViewFilterBy
  
import Network.AWS.ServiceCatalog.Types.SearchFilterValue
  
import Network.AWS.ServiceCatalog.Types.ResourceChangeDetail
  
import Network.AWS.ServiceCatalog.Types.ProductViewFilterValue
  
import Network.AWS.ServiceCatalog.Types.UsageInstruction
  
import Network.AWS.ServiceCatalog.Types.ShareError
  
import Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionValue
  
import Network.AWS.ServiceCatalog.Types.TagKey
  
import Network.AWS.ServiceCatalog.Types.ProductViewFilterBy
  
import Network.AWS.ServiceCatalog.Types.DefaultValue
  
import Network.AWS.ServiceCatalog.Types.Region
  
import Network.AWS.ServiceCatalog.Types.NotificationArn
  
import Network.AWS.ServiceCatalog.Types.PropertyName
  
import Network.AWS.ServiceCatalog.Types.ExecutionParameterValue
  
import Network.AWS.ServiceCatalog.Types.OutputKey
  
import Network.AWS.ServiceCatalog.Types.UpdateProvisioningPreferences
  
import Network.AWS.ServiceCatalog.Types.PageToken
  
import Network.AWS.ServiceCatalog.Types.PhysicalId
  
import Network.AWS.ServiceCatalog.Types.ListTagOptionsFilters
  
import Network.AWS.ServiceCatalog.Types.SupportEmail
  
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactPropertyValue
  
import Network.AWS.ServiceCatalog.Types.Message
  
import Network.AWS.ServiceCatalog.Types.StackInstanceStatus
  
import Network.AWS.ServiceCatalog.Types.OrganizationNode
  
import Network.AWS.ServiceCatalog.Types.AllowedValue
  
import Network.AWS.ServiceCatalog.Types.ListRecordHistorySearchFilter
  
import Network.AWS.ServiceCatalog.Types.Replacement
  
import Network.AWS.ServiceCatalog.Types.ProvisionedProductId
  
import Network.AWS.ServiceCatalog.Types.Description
  
import Network.AWS.ServiceCatalog.Types.ProductSource
  
import Network.AWS.ServiceCatalog.Types.ProviderName
  
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactProperties
  
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactDetail
  
import Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionType
  
import Network.AWS.ServiceCatalog.Types.ResourceTargetDefinition
  
  
  
  
import Network.AWS.ServiceCatalog.Types.RecordStatus
  
import Network.AWS.ServiceCatalog.Types.ProductViewAggregationValue
  
import Network.AWS.ServiceCatalog.Types.SupportDescription
  
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactGuidance
  
import Network.AWS.ServiceCatalog.Types.ProvisioningPreferences
  
  
  
import Network.AWS.ServiceCatalog.Types.StackSetOperationType
  
import Network.AWS.ServiceCatalog.Types.NextPageToken
  
import Network.AWS.ServiceCatalog.Types.Name
  
import Network.AWS.ServiceCatalog.Types.Distributor
  
import Network.AWS.ServiceCatalog.Types.Owner
  
import Network.AWS.ServiceCatalog.Types.PortfolioId
  
import Network.AWS.ServiceCatalog.Types.RecordId
  
import Network.AWS.ServiceCatalog.Types.PathId
  
import Network.AWS.ServiceCatalog.Types.PathName
  
import Network.AWS.ServiceCatalog.Types.ProductId
  
import Network.AWS.ServiceCatalog.Types.ProductName
  
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactId
  
import Network.AWS.ServiceCatalog.Types.LaunchRoleArn
  
import Network.AWS.ServiceCatalog.Types.ConstraintId
  
import Network.AWS.ServiceCatalog.Types.Type
  
import Network.AWS.ServiceCatalog.Types.Key
  
import Network.AWS.ServiceCatalog.Types.Value
  
import Network.AWS.ServiceCatalog.Types.PlanId
  
import Network.AWS.ServiceCatalog.Types.ProvisionProductId
  
import Network.AWS.ServiceCatalog.Types.LastProvisioningRecordId
  
import Network.AWS.ServiceCatalog.Types.LastRecordId
  
import Network.AWS.ServiceCatalog.Types.LastSuccessfulProvisioningRecordId
  
import Network.AWS.ServiceCatalog.Types.ShortDescription
  
import Network.AWS.ServiceCatalog.Types.OrganizationParentId
  
import Network.AWS.ServiceCatalog.Types.Code
  
import Network.AWS.ServiceCatalog.Types.ServiceActionId
  
import Network.AWS.ServiceCatalog.Types.SortBy
  
import Network.AWS.ServiceCatalog.Types.ARN
  
import Network.AWS.ServiceCatalog.Types.CopyProductToken
  
import Network.AWS.ServiceCatalog.Types.PortfolioShareToken
  
import Network.AWS.ServiceCatalog.Types.SourceProductArn
  
import Network.AWS.ServiceCatalog.Types.TargetProductName
  

-- | API version @2015-12-10@ of the Amazon Service Catalog SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "ServiceCatalog",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "servicecatalog",
                 Core._svcVersion = "2015-12-10", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "ServiceCatalog",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | One or more parameters provided to the operation are not valid.
_InvalidParametersException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParametersException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParametersException"
{-# INLINEABLE _InvalidParametersException #-}
{-# DEPRECATED _InvalidParametersException "Use generic-lens or generic-optics instead"  #-}

-- | The specified resource is a duplicate.
_DuplicateResourceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateResourceException
  = Core._MatchServiceError mkServiceConfig
      "DuplicateResourceException"
{-# INLINEABLE _DuplicateResourceException #-}
{-# DEPRECATED _DuplicateResourceException "Use generic-lens or generic-optics instead"  #-}

-- | The operation is not supported.
_OperationNotSupportedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationNotSupportedException
  = Core._MatchServiceError mkServiceConfig
      "OperationNotSupportedException"
{-# INLINEABLE _OperationNotSupportedException #-}
{-# DEPRECATED _OperationNotSupportedException "Use generic-lens or generic-optics instead"  #-}

-- | An operation requiring TagOptions failed because the TagOptions migration process has not been performed for this account. Please use the AWS console to perform the migration process before retrying the operation.
_TagOptionNotMigratedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagOptionNotMigratedException
  = Core._MatchServiceError mkServiceConfig
      "TagOptionNotMigratedException"
{-# INLINEABLE _TagOptionNotMigratedException #-}
{-# DEPRECATED _TagOptionNotMigratedException "Use generic-lens or generic-optics instead"  #-}

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | An attempt was made to modify a resource that is in a state that is not valid. Check your resources to ensure that they are in valid states before retrying the operation.
_InvalidStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidStateException
  = Core._MatchServiceError mkServiceConfig "InvalidStateException"
{-# INLINEABLE _InvalidStateException #-}
{-# DEPRECATED _InvalidStateException "Use generic-lens or generic-optics instead"  #-}

-- | The current limits of the service would have been exceeded by this operation. Decrease your resource use or increase your service limits and retry the operation.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | A resource that is currently in use. Ensure that the resource is not in use and retry the operation.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException
  = Core._MatchServiceError mkServiceConfig "ResourceInUseException"
{-# INLINEABLE _ResourceInUseException #-}
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead"  #-}
