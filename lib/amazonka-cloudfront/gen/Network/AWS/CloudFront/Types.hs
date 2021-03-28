-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _TooManyOriginCustomHeaders
    , _InvalidTagging
    , _InvalidErrorCode
    , _NoSuchFieldLevelEncryptionProfile
    , _FieldLevelEncryptionProfileInUse
    , _InvalidOriginReadTimeout
    , _TooManyFieldLevelEncryptionProfiles
    , _TooManyCacheBehaviors
    , _TooManyCloudFrontOriginAccessIdentities
    , _RealtimeLogConfigAlreadyExists
    , _InvalidOriginAccessIdentity
    , _DistributionNotDisabled
    , _NoSuchStreamingDistribution
    , _InconsistentQuantities
    , _InvalidArgument
    , _InvalidOriginKeepaliveTimeout
    , _TooManyInvalidationsInProgress
    , _InvalidWebACLId
    , _TooManyQueryStringParameters
    , _TooManyFieldLevelEncryptionQueryArgProfiles
    , _TooManyDistributionCNAMEs
    , _NoSuchCloudFrontOriginAccessIdentity
    , _CloudFrontOriginAccessIdentityInUse
    , _TooManyStreamingDistributions
    , _CannotChangeImmutablePublicKeyFields
    , _BatchTooLarge
    , _TooManyCookieNamesInWhiteList
    , _TooManyPublicKeysInKeyGroup
    , _InvalidLambdaFunctionAssociation
    , _TooManyKeyGroupsAssociatedToDistribution
    , _NoSuchRealtimeLogConfig
    , _InvalidForwardCookies
    , _FieldLevelEncryptionConfigInUse
    , _TooManyTrustedSigners
    , _TooManyDistributionsAssociatedToKeyGroup
    , _InvalidOrigin
    , _CachePolicyInUse
    , _NoSuchInvalidation
    , _PublicKeyAlreadyExists
    , _NoSuchOrigin
    , _TooManyHeadersInCachePolicy
    , _NoSuchCachePolicy
    , _InvalidTTLOrder
    , _StreamingDistributionNotDisabled
    , _OriginRequestPolicyAlreadyExists
    , _TooManyHeadersInForwardedValues
    , _NoSuchResource
    , _TooManyFieldLevelEncryptionEncryptionEntities
    , _TooManyStreamingDistributionCNAMEs
    , _FieldLevelEncryptionProfileAlreadyExists
    , _KeyGroupAlreadyExists
    , _TrustedKeyGroupDoesNotExist
    , _ResourceInUse
    , _InvalidRequiredProtocol
    , _TooManyDistributions
    , _TooManyDistributionsWithSingleFunctionARN
    , _TooManyHeadersInOriginRequestPolicy
    , _TooManyCertificates
    , _NoSuchOriginRequestPolicy
    , _DistributionAlreadyExists
    , _TooManyDistributionsAssociatedToFieldLevelEncryptionConfig
    , _TooManyKeyGroups
    , _InvalidQueryStringParameters
    , _MissingBody
    , _IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior
    , _TooManyOriginRequestPolicies
    , _IllegalDelete
    , _IllegalUpdate
    , _InvalidIfMatchVersion
    , _FieldLevelEncryptionConfigAlreadyExists
    , _PreconditionFailed
    , _InvalidResponseCode
    , _InvalidHeadersForS3Origin
    , _CNAMEAlreadyExists
    , _NoSuchPublicKey
    , _PublicKeyInUse
    , _TrustedSignerDoesNotExist
    , _InvalidProtocolSettings
    , _CachePolicyAlreadyExists
    , _TooManyCookiesInOriginRequestPolicy
    , _TooManyOriginGroupsPerDistribution
    , _TooManyPublicKeys
    , _NoSuchFieldLevelEncryptionConfig
    , _TooManyRealtimeLogConfigs
    , _RealtimeLogConfigInUse
    , _TooManyCachePolicies
    , _TooManyFieldLevelEncryptionContentTypeProfiles
    , _TooManyFieldLevelEncryptionFieldPatterns
    , _TooManyFieldLevelEncryptionConfigs
    , _TooManyLambdaFunctionAssociations
    , _CloudFrontOriginAccessIdentityAlreadyExists
    , _TooManyQueryStringsInCachePolicy
    , _TooManyOrigins
    , _InvalidRelativePath
    , _StreamingDistributionAlreadyExists
    , _TooManyDistributionsAssociatedToOriginRequestPolicy
    , _QueryArgProfileEmpty
    , _TooManyCookiesInCachePolicy
    , _InvalidMinimumProtocolVersion
    , _AccessDenied
    , _InvalidViewerCertificate
    , _NoSuchDistribution
    , _FieldLevelEncryptionProfileSizeExceeded
    , _TooManyQueryStringsInOriginRequestPolicy
    , _InvalidDefaultRootObject
    , _TooManyDistributionsWithLambdaAssociations
    , _TooManyDistributionsAssociatedToCachePolicy
    , _InvalidGeoRestrictionParameter
    , _OriginRequestPolicyInUse
    , _InvalidLocationCode

    -- * EncryptionEntity
    , EncryptionEntity (..)
    , mkEncryptionEntity
    , eePublicKeyId
    , eeProviderId
    , eeFieldPatterns

    -- * CloudFrontOriginAccessIdentityList
    , CloudFrontOriginAccessIdentityList (..)
    , mkCloudFrontOriginAccessIdentityList
    , cfoailMarker
    , cfoailMaxItems
    , cfoailIsTruncated
    , cfoailQuantity
    , cfoailItems
    , cfoailNextMarker

    -- * CachePolicy
    , CachePolicy (..)
    , mkCachePolicy
    , cpId
    , cpLastModifiedTime
    , cpCachePolicyConfig

    -- * Invalidation
    , Invalidation (..)
    , mkInvalidation
    , iId
    , iStatus
    , iCreateTime
    , iInvalidationBatch

    -- * SSLSupportMethod
    , SSLSupportMethod (..)

    -- * AllowedMethods
    , AllowedMethods (..)
    , mkAllowedMethods
    , amQuantity
    , amItems
    , amCachedMethods

    -- * OriginGroupFailoverCriteria
    , OriginGroupFailoverCriteria (..)
    , mkOriginGroupFailoverCriteria
    , ogfcStatusCodes

    -- * AliasICPRecordal
    , AliasICPRecordal (..)
    , mkAliasICPRecordal
    , aicprCNAME
    , aicprICPRecordalStatus

    -- * CloudFrontOriginAccessIdentityConfig
    , CloudFrontOriginAccessIdentityConfig (..)
    , mkCloudFrontOriginAccessIdentityConfig
    , cfoaicCallerReference
    , cfoaicComment

    -- * Origin
    , Origin (..)
    , mkOrigin
    , oId
    , oDomainName
    , oConnectionAttempts
    , oConnectionTimeout
    , oCustomHeaders
    , oCustomOriginConfig
    , oOriginPath
    , oOriginShield
    , oS3OriginConfig

    -- * FieldLevelEncryptionProfileSummary
    , FieldLevelEncryptionProfileSummary (..)
    , mkFieldLevelEncryptionProfileSummary
    , flepsId
    , flepsLastModifiedTime
    , flepsName
    , flepsEncryptionEntities
    , flepsComment

    -- * ViewerProtocolPolicy
    , ViewerProtocolPolicy (..)

    -- * StreamingDistributionList
    , StreamingDistributionList (..)
    , mkStreamingDistributionList
    , sdlMarker
    , sdlMaxItems
    , sdlIsTruncated
    , sdlQuantity
    , sdlItems
    , sdlNextMarker

    -- * KeyGroupSummary
    , KeyGroupSummary (..)
    , mkKeyGroupSummary
    , kgsKeyGroup

    -- * StreamingDistributionConfig
    , StreamingDistributionConfig (..)
    , mkStreamingDistributionConfig
    , sdcCallerReference
    , sdcS3Origin
    , sdcComment
    , sdcTrustedSigners
    , sdcEnabled
    , sdcAliases
    , sdcLogging
    , sdcPriceClass

    -- * Signer
    , Signer (..)
    , mkSigner
    , sAwsAccountNumber
    , sKeyPairIds

    -- * CookiePreference
    , CookiePreference (..)
    , mkCookiePreference
    , cpForward
    , cpWhitelistedNames

    -- * QueryArgProfileConfig
    , QueryArgProfileConfig (..)
    , mkQueryArgProfileConfig
    , qapcForwardWhenQueryArgProfileIsUnknown
    , qapcQueryArgProfiles

    -- * CustomHeaders
    , CustomHeaders (..)
    , mkCustomHeaders
    , chQuantity
    , chItems

    -- * CachePolicyHeaderBehavior
    , CachePolicyHeaderBehavior (..)

    -- * LambdaFunctionAssociations
    , LambdaFunctionAssociations (..)
    , mkLambdaFunctionAssociations
    , lfaQuantity
    , lfaItems

    -- * QueryStringCacheKeys
    , QueryStringCacheKeys (..)
    , mkQueryStringCacheKeys
    , qsckQuantity
    , qsckItems

    -- * CachePolicyQueryStringBehavior
    , CachePolicyQueryStringBehavior (..)

    -- * OriginRequestPolicyList
    , OriginRequestPolicyList (..)
    , mkOriginRequestPolicyList
    , orplMaxItems
    , orplQuantity
    , orplItems
    , orplNextMarker

    -- * FieldLevelEncryptionSummary
    , FieldLevelEncryptionSummary (..)
    , mkFieldLevelEncryptionSummary
    , flesId
    , flesLastModifiedTime
    , flesComment
    , flesContentTypeProfileConfig
    , flesQueryArgProfileConfig

    -- * OriginRequestPolicyConfig
    , OriginRequestPolicyConfig (..)
    , mkOriginRequestPolicyConfig
    , orpcName
    , orpcHeadersConfig
    , orpcCookiesConfig
    , orpcQueryStringsConfig
    , orpcComment

    -- * MonitoringSubscription
    , MonitoringSubscription (..)
    , mkMonitoringSubscription
    , msRealtimeMetricsSubscriptionConfig

    -- * ActiveTrustedKeyGroups
    , ActiveTrustedKeyGroups (..)
    , mkActiveTrustedKeyGroups
    , atkgEnabled
    , atkgQuantity
    , atkgItems

    -- * OriginProtocolPolicy
    , OriginProtocolPolicy (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * ContentTypeProfile
    , ContentTypeProfile (..)
    , mkContentTypeProfile
    , ctpFormat
    , ctpContentType
    , ctpProfileId

    -- * Distribution
    , Distribution (..)
    , mkDistribution
    , dId
    , dARN
    , dStatus
    , dLastModifiedTime
    , dInProgressInvalidationBatches
    , dDomainName
    , dDistributionConfig
    , dActiveTrustedKeyGroups
    , dActiveTrustedSigners
    , dAliasICPRecordals

    -- * HttpVersion
    , HttpVersion (..)

    -- * FieldLevelEncryptionProfileList
    , FieldLevelEncryptionProfileList (..)
    , mkFieldLevelEncryptionProfileList
    , fleplMaxItems
    , fleplQuantity
    , fleplItems
    , fleplNextMarker

    -- * SslProtocol
    , SslProtocol (..)

    -- * KeyGroupConfig
    , KeyGroupConfig (..)
    , mkKeyGroupConfig
    , kgcName
    , kgcItems
    , kgcComment

    -- * RealtimeLogConfigs
    , RealtimeLogConfigs (..)
    , mkRealtimeLogConfigs
    , rlcMaxItems
    , rlcIsTruncated
    , rlcMarker
    , rlcItems
    , rlcNextMarker

    -- * FieldLevelEncryptionProfileConfig
    , FieldLevelEncryptionProfileConfig (..)
    , mkFieldLevelEncryptionProfileConfig
    , flepcName
    , flepcCallerReference
    , flepcEncryptionEntities
    , flepcComment

    -- * CloudFrontOriginAccessIdentitySummary
    , CloudFrontOriginAccessIdentitySummary (..)
    , mkCloudFrontOriginAccessIdentitySummary
    , cfoaisId
    , cfoaisS3CanonicalUserId
    , cfoaisComment

    -- * KeyGroupList
    , KeyGroupList (..)
    , mkKeyGroupList
    , kglMaxItems
    , kglQuantity
    , kglItems
    , kglNextMarker

    -- * StreamingDistributionSummary
    , StreamingDistributionSummary (..)
    , mkStreamingDistributionSummary
    , sdsId
    , sdsARN
    , sdsStatus
    , sdsLastModifiedTime
    , sdsDomainName
    , sdsS3Origin
    , sdsAliases
    , sdsTrustedSigners
    , sdsComment
    , sdsPriceClass
    , sdsEnabled

    -- * OriginRequestPolicy
    , OriginRequestPolicy (..)
    , mkOriginRequestPolicy
    , orpId
    , orpLastModifiedTime
    , orpOriginRequestPolicyConfig

    -- * CustomOriginConfig
    , CustomOriginConfig (..)
    , mkCustomOriginConfig
    , cocHTTPPort
    , cocHTTPSPort
    , cocOriginProtocolPolicy
    , cocOriginKeepaliveTimeout
    , cocOriginReadTimeout
    , cocOriginSslProtocols

    -- * QueryArgProfile
    , QueryArgProfile (..)
    , mkQueryArgProfile
    , qapQueryArg
    , qapProfileId

    -- * FieldLevelEncryptionProfile
    , FieldLevelEncryptionProfile (..)
    , mkFieldLevelEncryptionProfile
    , flepId
    , flepLastModifiedTime
    , flepFieldLevelEncryptionProfileConfig

    -- * OriginRequestPolicyCookiesConfig
    , OriginRequestPolicyCookiesConfig (..)
    , mkOriginRequestPolicyCookiesConfig
    , orpccCookieBehavior
    , orpccCookies

    -- * OriginGroups
    , OriginGroups (..)
    , mkOriginGroups
    , ogQuantity
    , ogItems

    -- * Aliases
    , Aliases (..)
    , mkAliases
    , aQuantity
    , aItems

    -- * InvalidationBatch
    , InvalidationBatch (..)
    , mkInvalidationBatch
    , ibPaths
    , ibCallerReference

    -- * CertificateSource
    , CertificateSource (..)

    -- * InvalidationSummary
    , InvalidationSummary (..)
    , mkInvalidationSummary
    , isId
    , isCreateTime
    , isStatus

    -- * LambdaFunctionARN
    , LambdaFunctionARN (..)

    -- * DistributionConfig
    , DistributionConfig (..)
    , mkDistributionConfig
    , dcCallerReference
    , dcOrigins
    , dcDefaultCacheBehavior
    , dcComment
    , dcEnabled
    , dcAliases
    , dcCacheBehaviors
    , dcCustomErrorResponses
    , dcDefaultRootObject
    , dcHttpVersion
    , dcIsIPV6Enabled
    , dcLogging
    , dcOriginGroups
    , dcPriceClass
    , dcRestrictions
    , dcViewerCertificate
    , dcWebACLId

    -- * CachePolicySummary
    , CachePolicySummary (..)
    , mkCachePolicySummary
    , cpsType
    , cpsCachePolicy

    -- * CacheBehavior
    , CacheBehavior (..)
    , mkCacheBehavior
    , cbPathPattern
    , cbTargetOriginId
    , cbViewerProtocolPolicy
    , cbAllowedMethods
    , cbCachePolicyId
    , cbCompress
    , cbDefaultTTL
    , cbFieldLevelEncryptionId
    , cbForwardedValues
    , cbLambdaFunctionAssociations
    , cbMaxTTL
    , cbMinTTL
    , cbOriginRequestPolicyId
    , cbRealtimeLogConfigArn
    , cbSmoothStreaming
    , cbTrustedKeyGroups
    , cbTrustedSigners

    -- * KeyGroup
    , KeyGroup (..)
    , mkKeyGroup
    , kgId
    , kgLastModifiedTime
    , kgKeyGroupConfig

    -- * ContentTypeProfileConfig
    , ContentTypeProfileConfig (..)
    , mkContentTypeProfileConfig
    , ctpcForwardWhenContentTypeIsUnknown
    , ctpcContentTypeProfiles

    -- * ParametersInCacheKeyAndForwardedToOrigin
    , ParametersInCacheKeyAndForwardedToOrigin (..)
    , mkParametersInCacheKeyAndForwardedToOrigin
    , pickaftoEnableAcceptEncodingGzip
    , pickaftoHeadersConfig
    , pickaftoCookiesConfig
    , pickaftoQueryStringsConfig
    , pickaftoEnableAcceptEncodingBrotli

    -- * DistributionList
    , DistributionList (..)
    , mkDistributionList
    , dlMarker
    , dlMaxItems
    , dlIsTruncated
    , dlQuantity
    , dlItems
    , dlNextMarker

    -- * Format
    , Format (..)

    -- * KeyPairIds
    , KeyPairIds (..)
    , mkKeyPairIds
    , kpiQuantity
    , kpiItems

    -- * PriceClass
    , PriceClass (..)

    -- * OriginShieldRegion
    , OriginShieldRegion (..)

    -- * OriginGroup
    , OriginGroup (..)
    , mkOriginGroup
    , ogId
    , ogFailoverCriteria
    , ogMembers

    -- * CustomErrorResponses
    , CustomErrorResponses (..)
    , mkCustomErrorResponses
    , cerQuantity
    , cerItems

    -- * OriginRequestPolicyHeadersConfig
    , OriginRequestPolicyHeadersConfig (..)
    , mkOriginRequestPolicyHeadersConfig
    , orphcHeaderBehavior
    , orphcHeaders

    -- * ICPRecordalStatus
    , ICPRecordalStatus (..)

    -- * TagKeys
    , TagKeys (..)
    , mkTagKeys
    , tkItems

    -- * PublicKey
    , PublicKey (..)
    , mkPublicKey
    , pkId
    , pkCreatedTime
    , pkPublicKeyConfig

    -- * S3OriginConfig
    , S3OriginConfig (..)
    , mkS3OriginConfig
    , socOriginAccessIdentity

    -- * OriginRequestPolicyQueryStringBehavior
    , OriginRequestPolicyQueryStringBehavior (..)

    -- * KinesisStreamConfig
    , KinesisStreamConfig (..)
    , mkKinesisStreamConfig
    , kscRoleARN
    , kscStreamARN

    -- * TrustedKeyGroups
    , TrustedKeyGroups (..)
    , mkTrustedKeyGroups
    , tkgEnabled
    , tkgQuantity
    , tkgItems

    -- * GeoRestriction
    , GeoRestriction (..)
    , mkGeoRestriction
    , grRestrictionType
    , grQuantity
    , grItems

    -- * RealtimeLogConfig
    , RealtimeLogConfig (..)
    , mkRealtimeLogConfig
    , rlcARN
    , rlcName
    , rlcSamplingRate
    , rlcEndPoints
    , rlcFields

    -- * S3Origin
    , S3Origin (..)
    , mkS3Origin
    , soDomainName
    , soOriginAccessIdentity

    -- * PublicKeyList
    , PublicKeyList (..)
    , mkPublicKeyList
    , pklMaxItems
    , pklQuantity
    , pklItems
    , pklNextMarker

    -- * Headers
    , Headers (..)
    , mkHeaders
    , hQuantity
    , hItems

    -- * CachePolicyCookieBehavior
    , CachePolicyCookieBehavior (..)

    -- * PublicKeyConfig
    , PublicKeyConfig (..)
    , mkPublicKeyConfig
    , pkcCallerReference
    , pkcName
    , pkcEncodedKey
    , pkcComment

    -- * LambdaFunctionAssociation
    , LambdaFunctionAssociation (..)
    , mkLambdaFunctionAssociation
    , lfaLambdaFunctionARN
    , lfaEventType
    , lfaIncludeBody

    -- * EventType
    , EventType (..)

    -- * CachedMethods
    , CachedMethods (..)
    , mkCachedMethods
    , cmQuantity
    , cmItems

    -- * CachePolicyType
    , CachePolicyType (..)

    -- * ViewerCertificate
    , ViewerCertificate (..)
    , mkViewerCertificate
    , vcACMCertificateArn
    , vcCertificate
    , vcCertificateSource
    , vcCloudFrontDefaultCertificate
    , vcIAMCertificateId
    , vcMinimumProtocolVersion
    , vcSSLSupportMethod

    -- * RealtimeMetricsSubscriptionStatus
    , RealtimeMetricsSubscriptionStatus (..)

    -- * ResourceARN
    , ResourceARN (..)

    -- * Restrictions
    , Restrictions (..)
    , mkRestrictions
    , rGeoRestriction

    -- * KGKeyPairIds
    , KGKeyPairIds (..)
    , mkKGKeyPairIds
    , kgkpiKeyGroupId
    , kgkpiKeyPairIds

    -- * Origins
    , Origins (..)
    , mkOrigins
    , oQuantity
    , oItems

    -- * Method
    , Method (..)

    -- * StreamingDistributionConfigWithTags
    , StreamingDistributionConfigWithTags (..)
    , mkStreamingDistributionConfigWithTags
    , sdcwtStreamingDistributionConfig
    , sdcwtTags

    -- * MinimumProtocolVersion
    , MinimumProtocolVersion (..)

    -- * ForwardedValues
    , ForwardedValues (..)
    , mkForwardedValues
    , fvQueryString
    , fvCookies
    , fvHeaders
    , fvQueryStringCacheKeys

    -- * EncryptionEntities
    , EncryptionEntities (..)
    , mkEncryptionEntities
    , eeQuantity
    , eeItems

    -- * StatusCodes
    , StatusCodes (..)
    , mkStatusCodes
    , scQuantity
    , scItems

    -- * TrustedSigners
    , TrustedSigners (..)
    , mkTrustedSigners
    , tsEnabled
    , tsQuantity
    , tsItems

    -- * PublicKeySummary
    , PublicKeySummary (..)
    , mkPublicKeySummary
    , pksId
    , pksName
    , pksCreatedTime
    , pksEncodedKey
    , pksComment

    -- * CachePolicyQueryStringsConfig
    , CachePolicyQueryStringsConfig (..)
    , mkCachePolicyQueryStringsConfig
    , cpqscQueryStringBehavior
    , cpqscQueryStrings

    -- * CachePolicyHeadersConfig
    , CachePolicyHeadersConfig (..)
    , mkCachePolicyHeadersConfig
    , cphcHeaderBehavior
    , cphcHeaders

    -- * ItemSelection
    , ItemSelection (..)

    -- * OriginRequestPolicyCookieBehavior
    , OriginRequestPolicyCookieBehavior (..)

    -- * OriginSslProtocols
    , OriginSslProtocols (..)
    , mkOriginSslProtocols
    , ospQuantity
    , ospItems

    -- * DistributionIdList
    , DistributionIdList (..)
    , mkDistributionIdList
    , dilMarker
    , dilMaxItems
    , dilIsTruncated
    , dilQuantity
    , dilItems
    , dilNextMarker

    -- * OriginRequestPolicyType
    , OriginRequestPolicyType (..)

    -- * TagKey
    , TagKey (..)

    -- * StreamingLoggingConfig
    , StreamingLoggingConfig (..)
    , mkStreamingLoggingConfig
    , slcEnabled
    , slcBucket
    , slcPrefix

    -- * OriginGroupMembers
    , OriginGroupMembers (..)
    , mkOriginGroupMembers
    , ogmQuantity
    , ogmItems

    -- * OriginCustomHeader
    , OriginCustomHeader (..)
    , mkOriginCustomHeader
    , ochHeaderName
    , ochHeaderValue

    -- * CookieNames
    , CookieNames (..)
    , mkCookieNames
    , cnQuantity
    , cnItems

    -- * CustomErrorResponse
    , CustomErrorResponse (..)
    , mkCustomErrorResponse
    , cerErrorCode
    , cerErrorCachingMinTTL
    , cerResponseCode
    , cerResponsePagePath

    -- * EndPoint
    , EndPoint (..)
    , mkEndPoint
    , epStreamType
    , epKinesisStreamConfig

    -- * QueryStringNames
    , QueryStringNames (..)
    , mkQueryStringNames
    , qsnQuantity
    , qsnItems

    -- * FieldLevelEncryption
    , FieldLevelEncryption (..)
    , mkFieldLevelEncryption
    , fleId
    , fleLastModifiedTime
    , fleFieldLevelEncryptionConfig

    -- * DistributionConfigWithTags
    , DistributionConfigWithTags (..)
    , mkDistributionConfigWithTags
    , dcwtDistributionConfig
    , dcwtTags

    -- * OriginRequestPolicyQueryStringsConfig
    , OriginRequestPolicyQueryStringsConfig (..)
    , mkOriginRequestPolicyQueryStringsConfig
    , orpqscQueryStringBehavior
    , orpqscQueryStrings

    -- * OriginGroupMember
    , OriginGroupMember (..)
    , mkOriginGroupMember
    , ogmOriginId

    -- * OriginShield
    , OriginShield (..)
    , mkOriginShield
    , osEnabled
    , osOriginShieldRegion

    -- * CacheBehaviors
    , CacheBehaviors (..)
    , mkCacheBehaviors
    , cbQuantity
    , cbItems

    -- * DefaultCacheBehavior
    , DefaultCacheBehavior (..)
    , mkDefaultCacheBehavior
    , dcbTargetOriginId
    , dcbViewerProtocolPolicy
    , dcbAllowedMethods
    , dcbCachePolicyId
    , dcbCompress
    , dcbDefaultTTL
    , dcbFieldLevelEncryptionId
    , dcbForwardedValues
    , dcbLambdaFunctionAssociations
    , dcbMaxTTL
    , dcbMinTTL
    , dcbOriginRequestPolicyId
    , dcbRealtimeLogConfigArn
    , dcbSmoothStreaming
    , dcbTrustedKeyGroups
    , dcbTrustedSigners

    -- * InvalidationList
    , InvalidationList (..)
    , mkInvalidationList
    , ilMarker
    , ilMaxItems
    , ilIsTruncated
    , ilQuantity
    , ilItems
    , ilNextMarker

    -- * QueryArgProfiles
    , QueryArgProfiles (..)
    , mkQueryArgProfiles
    , qapQuantity
    , qapItems

    -- * CachePolicyConfig
    , CachePolicyConfig (..)
    , mkCachePolicyConfig
    , cpcName
    , cpcMinTTL
    , cpcComment
    , cpcDefaultTTL
    , cpcMaxTTL
    , cpcParametersInCacheKeyAndForwardedToOrigin

    -- * StreamingDistribution
    , StreamingDistribution (..)
    , mkStreamingDistribution
    , sdId
    , sdARN
    , sdStatus
    , sdDomainName
    , sdActiveTrustedSigners
    , sdStreamingDistributionConfig
    , sdLastModifiedTime

    -- * CachePolicyList
    , CachePolicyList (..)
    , mkCachePolicyList
    , cplMaxItems
    , cplQuantity
    , cplItems
    , cplNextMarker

    -- * Paths
    , Paths (..)
    , mkPaths
    , pQuantity
    , pItems

    -- * CloudFrontOriginAccessIdentity
    , CloudFrontOriginAccessIdentity (..)
    , mkCloudFrontOriginAccessIdentity
    , cfoaiId
    , cfoaiS3CanonicalUserId
    , cfoaiCloudFrontOriginAccessIdentityConfig

    -- * OriginRequestPolicyHeaderBehavior
    , OriginRequestPolicyHeaderBehavior (..)

    -- * ActiveTrustedSigners
    , ActiveTrustedSigners (..)
    , mkActiveTrustedSigners
    , atsEnabled
    , atsQuantity
    , atsItems

    -- * DistributionSummary
    , DistributionSummary (..)
    , mkDistributionSummary
    , dsId
    , dsARN
    , dsStatus
    , dsLastModifiedTime
    , dsDomainName
    , dsAliases
    , dsOrigins
    , dsDefaultCacheBehavior
    , dsCacheBehaviors
    , dsCustomErrorResponses
    , dsComment
    , dsPriceClass
    , dsEnabled
    , dsViewerCertificate
    , dsRestrictions
    , dsWebACLId
    , dsHttpVersion
    , dsIsIPV6Enabled
    , dsAliasICPRecordals
    , dsOriginGroups

    -- * OriginRequestPolicySummary
    , OriginRequestPolicySummary (..)
    , mkOriginRequestPolicySummary
    , orpsType
    , orpsOriginRequestPolicy

    -- * ContentTypeProfiles
    , ContentTypeProfiles (..)
    , mkContentTypeProfiles
    , ctpQuantity
    , ctpItems

    -- * Tags
    , Tags (..)
    , mkTags
    , tItems

    -- * CachePolicyCookiesConfig
    , CachePolicyCookiesConfig (..)
    , mkCachePolicyCookiesConfig
    , cpccCookieBehavior
    , cpccCookies

    -- * GeoRestrictionType
    , GeoRestrictionType (..)

    -- * LoggingConfig
    , LoggingConfig (..)
    , mkLoggingConfig
    , lcEnabled
    , lcIncludeCookies
    , lcBucket
    , lcPrefix

    -- * FieldLevelEncryptionConfig
    , FieldLevelEncryptionConfig (..)
    , mkFieldLevelEncryptionConfig
    , flecCallerReference
    , flecComment
    , flecContentTypeProfileConfig
    , flecQueryArgProfileConfig

    -- * FieldLevelEncryptionList
    , FieldLevelEncryptionList (..)
    , mkFieldLevelEncryptionList
    , flelMaxItems
    , flelQuantity
    , flelItems
    , flelNextMarker

    -- * FieldPatterns
    , FieldPatterns (..)
    , mkFieldPatterns
    , fpQuantity
    , fpItems

    -- * RealtimeMetricsSubscriptionConfig
    , RealtimeMetricsSubscriptionConfig (..)
    , mkRealtimeMetricsSubscriptionConfig
    , rmscRealtimeMetricsSubscriptionStatus

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * Comment
    , Comment (..)

    -- * Resource
    , Resource (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.CloudFront.Types.EncryptionEntity
  
import Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityList
  
import Network.AWS.CloudFront.Types.CachePolicy
  
import Network.AWS.CloudFront.Types.Invalidation
  
  
import Network.AWS.CloudFront.Types.SSLSupportMethod
  
import Network.AWS.CloudFront.Types.AllowedMethods
  
  
import Network.AWS.CloudFront.Types.OriginGroupFailoverCriteria
  
import Network.AWS.CloudFront.Types.AliasICPRecordal
  
import Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityConfig
  
  
import Network.AWS.CloudFront.Types.Origin
  
import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileSummary
  
  
import Network.AWS.CloudFront.Types.ViewerProtocolPolicy
  
  
import Network.AWS.CloudFront.Types.StreamingDistributionList
  
import Network.AWS.CloudFront.Types.KeyGroupSummary
  
  
  
import Network.AWS.CloudFront.Types.StreamingDistributionConfig
  
  
import Network.AWS.CloudFront.Types.Signer
  
  
import Network.AWS.CloudFront.Types.CookiePreference
  
import Network.AWS.CloudFront.Types.QueryArgProfileConfig
  
import Network.AWS.CloudFront.Types.CustomHeaders
  
import Network.AWS.CloudFront.Types.CachePolicyHeaderBehavior
  
  
import Network.AWS.CloudFront.Types.LambdaFunctionAssociations
  
import Network.AWS.CloudFront.Types.QueryStringCacheKeys
  
  
import Network.AWS.CloudFront.Types.CachePolicyQueryStringBehavior
  
import Network.AWS.CloudFront.Types.OriginRequestPolicyList
  
  
import Network.AWS.CloudFront.Types.FieldLevelEncryptionSummary
  
import Network.AWS.CloudFront.Types.OriginRequestPolicyConfig
  
import Network.AWS.CloudFront.Types.MonitoringSubscription
  
  
  
import Network.AWS.CloudFront.Types.ActiveTrustedKeyGroups
  
import Network.AWS.CloudFront.Types.OriginProtocolPolicy
  
import Network.AWS.CloudFront.Types.Tag
  
import Network.AWS.CloudFront.Types.ContentTypeProfile
  
import Network.AWS.CloudFront.Types.Distribution
  
  
import Network.AWS.CloudFront.Types.HttpVersion
  
import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileList
  
import Network.AWS.CloudFront.Types.SslProtocol
  
import Network.AWS.CloudFront.Types.KeyGroupConfig
  
import Network.AWS.CloudFront.Types.RealtimeLogConfigs
  
  
  
  
  
  
import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfileConfig
  
import Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentitySummary
  
  
  
  
  
  
import Network.AWS.CloudFront.Types.KeyGroupList
  
import Network.AWS.CloudFront.Types.StreamingDistributionSummary
  
  
  
  
  
  
import Network.AWS.CloudFront.Types.OriginRequestPolicy
  
  
  
import Network.AWS.CloudFront.Types.CustomOriginConfig
  
  
  
  
import Network.AWS.CloudFront.Types.QueryArgProfile
  
  
  
import Network.AWS.CloudFront.Types.FieldLevelEncryptionProfile
  
import Network.AWS.CloudFront.Types.OriginRequestPolicyCookiesConfig
  
import Network.AWS.CloudFront.Types.OriginGroups
  
import Network.AWS.CloudFront.Types.Aliases
  
  
import Network.AWS.CloudFront.Types.InvalidationBatch
  
import Network.AWS.CloudFront.Types.CertificateSource
  
  
import Network.AWS.CloudFront.Types.InvalidationSummary
  
import Network.AWS.CloudFront.Types.LambdaFunctionARN
  
import Network.AWS.CloudFront.Types.DistributionConfig
  
import Network.AWS.CloudFront.Types.CachePolicySummary
  
  
  
import Network.AWS.CloudFront.Types.CacheBehavior
  
  
import Network.AWS.CloudFront.Types.KeyGroup
  
import Network.AWS.CloudFront.Types.ContentTypeProfileConfig
  
import Network.AWS.CloudFront.Types.ParametersInCacheKeyAndForwardedToOrigin
  
import Network.AWS.CloudFront.Types.DistributionList
  
import Network.AWS.CloudFront.Types.Format
  
  
import Network.AWS.CloudFront.Types.KeyPairIds
  
import Network.AWS.CloudFront.Types.PriceClass
  
  
import Network.AWS.CloudFront.Types.OriginShieldRegion
  
import Network.AWS.CloudFront.Types.OriginGroup
  
import Network.AWS.CloudFront.Types.CustomErrorResponses
  
  
import Network.AWS.CloudFront.Types.OriginRequestPolicyHeadersConfig
  
import Network.AWS.CloudFront.Types.ICPRecordalStatus
  
import Network.AWS.CloudFront.Types.TagKeys
  
  
import Network.AWS.CloudFront.Types.PublicKey
  
  
import Network.AWS.CloudFront.Types.S3OriginConfig
  
import Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringBehavior
  
  
  
  
import Network.AWS.CloudFront.Types.KinesisStreamConfig
  
  
  
  
  
  
import Network.AWS.CloudFront.Types.TrustedKeyGroups
  
  
  
import Network.AWS.CloudFront.Types.GeoRestriction
  
  
import Network.AWS.CloudFront.Types.RealtimeLogConfig
  
  
import Network.AWS.CloudFront.Types.S3Origin
  
import Network.AWS.CloudFront.Types.PublicKeyList
  
import Network.AWS.CloudFront.Types.Headers
  
  
  
import Network.AWS.CloudFront.Types.CachePolicyCookieBehavior
  
  
  
  
import Network.AWS.CloudFront.Types.PublicKeyConfig
  
  
  
import Network.AWS.CloudFront.Types.LambdaFunctionAssociation
  
import Network.AWS.CloudFront.Types.EventType
  
import Network.AWS.CloudFront.Types.CachedMethods
  
import Network.AWS.CloudFront.Types.CachePolicyType
  
import Network.AWS.CloudFront.Types.ViewerCertificate
  
  
import Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionStatus
  
  
import Network.AWS.CloudFront.Types.ResourceARN
  
import Network.AWS.CloudFront.Types.Restrictions
  
import Network.AWS.CloudFront.Types.KGKeyPairIds
  
import Network.AWS.CloudFront.Types.Origins
  
  
  
  
import Network.AWS.CloudFront.Types.Method
  
import Network.AWS.CloudFront.Types.StreamingDistributionConfigWithTags
  
  
import Network.AWS.CloudFront.Types.MinimumProtocolVersion
  
import Network.AWS.CloudFront.Types.ForwardedValues
  
import Network.AWS.CloudFront.Types.EncryptionEntities
  
import Network.AWS.CloudFront.Types.StatusCodes
  
import Network.AWS.CloudFront.Types.TrustedSigners
  
import Network.AWS.CloudFront.Types.PublicKeySummary
  
  
  
  
  
  
  
  
  
  
import Network.AWS.CloudFront.Types.CachePolicyQueryStringsConfig
  
  
import Network.AWS.CloudFront.Types.CachePolicyHeadersConfig
  
import Network.AWS.CloudFront.Types.ItemSelection
  
  
  
import Network.AWS.CloudFront.Types.OriginRequestPolicyCookieBehavior
  
  
  
import Network.AWS.CloudFront.Types.OriginSslProtocols
  
  
  
import Network.AWS.CloudFront.Types.DistributionIdList
  
import Network.AWS.CloudFront.Types.OriginRequestPolicyType
  
  
import Network.AWS.CloudFront.Types.TagKey
  
import Network.AWS.CloudFront.Types.StreamingLoggingConfig
  
import Network.AWS.CloudFront.Types.OriginGroupMembers
  
  
import Network.AWS.CloudFront.Types.OriginCustomHeader
  
  
  
  
  
  
import Network.AWS.CloudFront.Types.CookieNames
  
import Network.AWS.CloudFront.Types.CustomErrorResponse
  
  
  
import Network.AWS.CloudFront.Types.EndPoint
  
import Network.AWS.CloudFront.Types.QueryStringNames
  
  
import Network.AWS.CloudFront.Types.FieldLevelEncryption
  
import Network.AWS.CloudFront.Types.DistributionConfigWithTags
  
import Network.AWS.CloudFront.Types.OriginRequestPolicyQueryStringsConfig
  
import Network.AWS.CloudFront.Types.OriginGroupMember
  
  
import Network.AWS.CloudFront.Types.OriginShield
  
  
import Network.AWS.CloudFront.Types.CacheBehaviors
  
import Network.AWS.CloudFront.Types.DefaultCacheBehavior
  
import Network.AWS.CloudFront.Types.InvalidationList
  
import Network.AWS.CloudFront.Types.QueryArgProfiles
  
  
import Network.AWS.CloudFront.Types.CachePolicyConfig
  
import Network.AWS.CloudFront.Types.StreamingDistribution
  
import Network.AWS.CloudFront.Types.CachePolicyList
  
  
import Network.AWS.CloudFront.Types.Paths
  
import Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentity
  
import Network.AWS.CloudFront.Types.OriginRequestPolicyHeaderBehavior
  
import Network.AWS.CloudFront.Types.ActiveTrustedSigners
  
import Network.AWS.CloudFront.Types.DistributionSummary
  
  
  
  
import Network.AWS.CloudFront.Types.OriginRequestPolicySummary
  
  
import Network.AWS.CloudFront.Types.ContentTypeProfiles
  
import Network.AWS.CloudFront.Types.Tags
  
  
import Network.AWS.CloudFront.Types.CachePolicyCookiesConfig
  
import Network.AWS.CloudFront.Types.GeoRestrictionType
  
  
  
import Network.AWS.CloudFront.Types.LoggingConfig
  
import Network.AWS.CloudFront.Types.FieldLevelEncryptionConfig
  
  
import Network.AWS.CloudFront.Types.FieldLevelEncryptionList
  
import Network.AWS.CloudFront.Types.FieldPatterns
  
import Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionConfig
  
import Network.AWS.CloudFront.Types.Key
  
import Network.AWS.CloudFront.Types.Value
  
import Network.AWS.CloudFront.Types.Comment
  
import Network.AWS.CloudFront.Types.Resource
  

-- | API version @2020-05-31@ of the Amazon CloudFront SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "CloudFront",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "cloudfront",
                 Core._svcVersion = "2020-05-31", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseXMLError "CloudFront",
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

-- | Your request contains too many origin custom headers.
_TooManyOriginCustomHeaders :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyOriginCustomHeaders
  = Core._MatchServiceError mkServiceConfig
      "TooManyOriginCustomHeaders"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyOriginCustomHeaders #-}
{-# DEPRECATED _TooManyOriginCustomHeaders "Use generic-lens or generic-optics instead"  #-}

-- | The tagging specified is not valid.
_InvalidTagging :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTagging
  = Core._MatchServiceError mkServiceConfig "InvalidTagging" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidTagging #-}
{-# DEPRECATED _InvalidTagging "Use generic-lens or generic-optics instead"  #-}

-- | An invalid error code was specified.
_InvalidErrorCode :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidErrorCode
  = Core._MatchServiceError mkServiceConfig "InvalidErrorCode" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidErrorCode #-}
{-# DEPRECATED _InvalidErrorCode "Use generic-lens or generic-optics instead"  #-}

-- | The specified profile for field-level encryption doesn't exist.
_NoSuchFieldLevelEncryptionProfile :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchFieldLevelEncryptionProfile
  = Core._MatchServiceError mkServiceConfig
      "NoSuchFieldLevelEncryptionProfile"
      Core.. Core.hasStatues 404
{-# INLINEABLE _NoSuchFieldLevelEncryptionProfile #-}
{-# DEPRECATED _NoSuchFieldLevelEncryptionProfile "Use generic-lens or generic-optics instead"  #-}

-- | The specified profile for field-level encryption is in use.
_FieldLevelEncryptionProfileInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FieldLevelEncryptionProfileInUse
  = Core._MatchServiceError mkServiceConfig
      "FieldLevelEncryptionProfileInUse"
      Core.. Core.hasStatues 409
{-# INLINEABLE _FieldLevelEncryptionProfileInUse #-}
{-# DEPRECATED _FieldLevelEncryptionProfileInUse "Use generic-lens or generic-optics instead"  #-}

-- | The read timeout specified for the origin is not valid.
_InvalidOriginReadTimeout :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOriginReadTimeout
  = Core._MatchServiceError mkServiceConfig
      "InvalidOriginReadTimeout"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidOriginReadTimeout #-}
{-# DEPRECATED _InvalidOriginReadTimeout "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of profiles for field-level encryption have been created.
_TooManyFieldLevelEncryptionProfiles :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyFieldLevelEncryptionProfiles
  = Core._MatchServiceError mkServiceConfig
      "TooManyFieldLevelEncryptionProfiles"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyFieldLevelEncryptionProfiles #-}
{-# DEPRECATED _TooManyFieldLevelEncryptionProfiles "Use generic-lens or generic-optics instead"  #-}

-- | You cannot create more cache behaviors for the distribution.
_TooManyCacheBehaviors :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyCacheBehaviors
  = Core._MatchServiceError mkServiceConfig "TooManyCacheBehaviors"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyCacheBehaviors #-}
{-# DEPRECATED _TooManyCacheBehaviors "Use generic-lens or generic-optics instead"  #-}

-- | Processing your request would cause you to exceed the maximum number of origin access identities allowed.
_TooManyCloudFrontOriginAccessIdentities :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyCloudFrontOriginAccessIdentities
  = Core._MatchServiceError mkServiceConfig
      "TooManyCloudFrontOriginAccessIdentities"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyCloudFrontOriginAccessIdentities #-}
{-# DEPRECATED _TooManyCloudFrontOriginAccessIdentities "Use generic-lens or generic-optics instead"  #-}

-- | A real-time log configuration with this name already exists. You must provide a unique name. To modify an existing real-time log configuration, use @UpdateRealtimeLogConfig@ .
_RealtimeLogConfigAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RealtimeLogConfigAlreadyExists
  = Core._MatchServiceError mkServiceConfig
      "RealtimeLogConfigAlreadyExists"
      Core.. Core.hasStatues 409
{-# INLINEABLE _RealtimeLogConfigAlreadyExists #-}
{-# DEPRECATED _RealtimeLogConfigAlreadyExists "Use generic-lens or generic-optics instead"  #-}

-- | The origin access identity is not valid or doesn't exist.
_InvalidOriginAccessIdentity :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOriginAccessIdentity
  = Core._MatchServiceError mkServiceConfig
      "InvalidOriginAccessIdentity"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidOriginAccessIdentity #-}
{-# DEPRECATED _InvalidOriginAccessIdentity "Use generic-lens or generic-optics instead"  #-}

-- | The specified CloudFront distribution is not disabled. You must disable the distribution before you can delete it.
_DistributionNotDisabled :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DistributionNotDisabled
  = Core._MatchServiceError mkServiceConfig "DistributionNotDisabled"
      Core.. Core.hasStatues 409
{-# INLINEABLE _DistributionNotDisabled #-}
{-# DEPRECATED _DistributionNotDisabled "Use generic-lens or generic-optics instead"  #-}

-- | The specified streaming distribution does not exist.
_NoSuchStreamingDistribution :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchStreamingDistribution
  = Core._MatchServiceError mkServiceConfig
      "NoSuchStreamingDistribution"
      Core.. Core.hasStatues 404
{-# INLINEABLE _NoSuchStreamingDistribution #-}
{-# DEPRECATED _NoSuchStreamingDistribution "Use generic-lens or generic-optics instead"  #-}

-- | The value of @Quantity@ and the size of @Items@ don't match.
_InconsistentQuantities :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InconsistentQuantities
  = Core._MatchServiceError mkServiceConfig "InconsistentQuantities"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InconsistentQuantities #-}
{-# DEPRECATED _InconsistentQuantities "Use generic-lens or generic-optics instead"  #-}

-- | An argument is invalid.
_InvalidArgument :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArgument
  = Core._MatchServiceError mkServiceConfig "InvalidArgument" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidArgument #-}
{-# DEPRECATED _InvalidArgument "Use generic-lens or generic-optics instead"  #-}

-- | The keep alive timeout specified for the origin is not valid.
_InvalidOriginKeepaliveTimeout :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOriginKeepaliveTimeout
  = Core._MatchServiceError mkServiceConfig
      "InvalidOriginKeepaliveTimeout"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidOriginKeepaliveTimeout #-}
{-# DEPRECATED _InvalidOriginKeepaliveTimeout "Use generic-lens or generic-optics instead"  #-}

-- | You have exceeded the maximum number of allowable InProgress invalidation batch requests, or invalidation objects.
_TooManyInvalidationsInProgress :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyInvalidationsInProgress
  = Core._MatchServiceError mkServiceConfig
      "TooManyInvalidationsInProgress"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyInvalidationsInProgress #-}
{-# DEPRECATED _TooManyInvalidationsInProgress "Use generic-lens or generic-optics instead"  #-}

-- | A web ACL ID specified is not valid. To specify a web ACL created using the latest version of AWS WAF, use the ACL ARN, for example @arn:aws:wafv2:us-east-1:123456789012:global/webacl/ExampleWebACL/473e64fd-f30b-4765-81a0-62ad96dd167a@ . To specify a web ACL created using AWS WAF Classic, use the ACL ID, for example @473e64fd-f30b-4765-81a0-62ad96dd167a@ .
_InvalidWebACLId :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidWebACLId
  = Core._MatchServiceError mkServiceConfig "InvalidWebACLId" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidWebACLId #-}
{-# DEPRECATED _InvalidWebACLId "Use generic-lens or generic-optics instead"  #-}

-- | Your request contains too many query string parameters.
_TooManyQueryStringParameters :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyQueryStringParameters
  = Core._MatchServiceError mkServiceConfig
      "TooManyQueryStringParameters"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyQueryStringParameters #-}
{-# DEPRECATED _TooManyQueryStringParameters "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of query arg profiles for field-level encryption have been created.
_TooManyFieldLevelEncryptionQueryArgProfiles :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyFieldLevelEncryptionQueryArgProfiles
  = Core._MatchServiceError mkServiceConfig
      "TooManyFieldLevelEncryptionQueryArgProfiles"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyFieldLevelEncryptionQueryArgProfiles #-}
{-# DEPRECATED _TooManyFieldLevelEncryptionQueryArgProfiles "Use generic-lens or generic-optics instead"  #-}

-- | Your request contains more CNAMEs than are allowed per distribution.
_TooManyDistributionCNAMEs :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyDistributionCNAMEs
  = Core._MatchServiceError mkServiceConfig
      "TooManyDistributionCNAMEs"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyDistributionCNAMEs #-}
{-# DEPRECATED _TooManyDistributionCNAMEs "Use generic-lens or generic-optics instead"  #-}

-- | The specified origin access identity does not exist.
_NoSuchCloudFrontOriginAccessIdentity :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchCloudFrontOriginAccessIdentity
  = Core._MatchServiceError mkServiceConfig
      "NoSuchCloudFrontOriginAccessIdentity"
      Core.. Core.hasStatues 404
{-# INLINEABLE _NoSuchCloudFrontOriginAccessIdentity #-}
{-# DEPRECATED _NoSuchCloudFrontOriginAccessIdentity "Use generic-lens or generic-optics instead"  #-}

-- | The Origin Access Identity specified is already in use.
_CloudFrontOriginAccessIdentityInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudFrontOriginAccessIdentityInUse
  = Core._MatchServiceError mkServiceConfig
      "CloudFrontOriginAccessIdentityInUse"
      Core.. Core.hasStatues 409
{-# INLINEABLE _CloudFrontOriginAccessIdentityInUse #-}
{-# DEPRECATED _CloudFrontOriginAccessIdentityInUse "Use generic-lens or generic-optics instead"  #-}

-- | Processing your request would cause you to exceed the maximum number of streaming distributions allowed.
_TooManyStreamingDistributions :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyStreamingDistributions
  = Core._MatchServiceError mkServiceConfig
      "TooManyStreamingDistributions"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyStreamingDistributions #-}
{-# DEPRECATED _TooManyStreamingDistributions "Use generic-lens or generic-optics instead"  #-}

-- | You can't change the value of a public key.
_CannotChangeImmutablePublicKeyFields :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CannotChangeImmutablePublicKeyFields
  = Core._MatchServiceError mkServiceConfig
      "CannotChangeImmutablePublicKeyFields"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CannotChangeImmutablePublicKeyFields #-}
{-# DEPRECATED _CannotChangeImmutablePublicKeyFields "Use generic-lens or generic-optics instead"  #-}

-- | Invalidation batch specified is too large.
_BatchTooLarge :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BatchTooLarge
  = Core._MatchServiceError mkServiceConfig "BatchTooLarge" Core..
      Core.hasStatues 413
{-# INLINEABLE _BatchTooLarge #-}
{-# DEPRECATED _BatchTooLarge "Use generic-lens or generic-optics instead"  #-}

-- | Your request contains more cookie names in the whitelist than are allowed per cache behavior.
_TooManyCookieNamesInWhiteList :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyCookieNamesInWhiteList
  = Core._MatchServiceError mkServiceConfig
      "TooManyCookieNamesInWhiteList"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyCookieNamesInWhiteList #-}
{-# DEPRECATED _TooManyCookieNamesInWhiteList "Use generic-lens or generic-optics instead"  #-}

-- | The number of public keys in this key group is more than the maximum allowed. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas> (formerly known as limits) in the /Amazon CloudFront Developer Guide/ .
_TooManyPublicKeysInKeyGroup :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyPublicKeysInKeyGroup
  = Core._MatchServiceError mkServiceConfig
      "TooManyPublicKeysInKeyGroup"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyPublicKeysInKeyGroup #-}
{-# DEPRECATED _TooManyPublicKeysInKeyGroup "Use generic-lens or generic-optics instead"  #-}

-- | The specified Lambda function association is invalid.
_InvalidLambdaFunctionAssociation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLambdaFunctionAssociation
  = Core._MatchServiceError mkServiceConfig
      "InvalidLambdaFunctionAssociation"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidLambdaFunctionAssociation #-}
{-# DEPRECATED _InvalidLambdaFunctionAssociation "Use generic-lens or generic-optics instead"  #-}

-- | The number of key groups referenced by this distribution is more than the maximum allowed. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas> (formerly known as limits) in the /Amazon CloudFront Developer Guide/ .
_TooManyKeyGroupsAssociatedToDistribution :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyKeyGroupsAssociatedToDistribution
  = Core._MatchServiceError mkServiceConfig
      "TooManyKeyGroupsAssociatedToDistribution"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyKeyGroupsAssociatedToDistribution #-}
{-# DEPRECATED _TooManyKeyGroupsAssociatedToDistribution "Use generic-lens or generic-optics instead"  #-}

-- | The real-time log configuration does not exist.
_NoSuchRealtimeLogConfig :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchRealtimeLogConfig
  = Core._MatchServiceError mkServiceConfig "NoSuchRealtimeLogConfig"
      Core.. Core.hasStatues 404
{-# INLINEABLE _NoSuchRealtimeLogConfig #-}
{-# DEPRECATED _NoSuchRealtimeLogConfig "Use generic-lens or generic-optics instead"  #-}

-- | Your request contains forward cookies option which doesn't match with the expectation for the @whitelisted@ list of cookie names. Either list of cookie names has been specified when not allowed or list of cookie names is missing when expected.
_InvalidForwardCookies :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidForwardCookies
  = Core._MatchServiceError mkServiceConfig "InvalidForwardCookies"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidForwardCookies #-}
{-# DEPRECATED _InvalidForwardCookies "Use generic-lens or generic-optics instead"  #-}

-- | The specified configuration for field-level encryption is in use.
_FieldLevelEncryptionConfigInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FieldLevelEncryptionConfigInUse
  = Core._MatchServiceError mkServiceConfig
      "FieldLevelEncryptionConfigInUse"
      Core.. Core.hasStatues 409
{-# INLINEABLE _FieldLevelEncryptionConfigInUse #-}
{-# DEPRECATED _FieldLevelEncryptionConfigInUse "Use generic-lens or generic-optics instead"  #-}

-- | Your request contains more trusted signers than are allowed per distribution.
_TooManyTrustedSigners :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTrustedSigners
  = Core._MatchServiceError mkServiceConfig "TooManyTrustedSigners"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyTrustedSigners #-}
{-# DEPRECATED _TooManyTrustedSigners "Use generic-lens or generic-optics instead"  #-}

-- | The number of distributions that reference this key group is more than the maximum allowed. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas> (formerly known as limits) in the /Amazon CloudFront Developer Guide/ .
_TooManyDistributionsAssociatedToKeyGroup :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyDistributionsAssociatedToKeyGroup
  = Core._MatchServiceError mkServiceConfig
      "TooManyDistributionsAssociatedToKeyGroup"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyDistributionsAssociatedToKeyGroup #-}
{-# DEPRECATED _TooManyDistributionsAssociatedToKeyGroup "Use generic-lens or generic-optics instead"  #-}

-- | The Amazon S3 origin server specified does not refer to a valid Amazon S3 bucket.
_InvalidOrigin :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOrigin
  = Core._MatchServiceError mkServiceConfig "InvalidOrigin" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidOrigin #-}
{-# DEPRECATED _InvalidOrigin "Use generic-lens or generic-optics instead"  #-}

-- | Cannot delete the cache policy because it is attached to one or more cache behaviors.
_CachePolicyInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CachePolicyInUse
  = Core._MatchServiceError mkServiceConfig "CachePolicyInUse" Core..
      Core.hasStatues 409
{-# INLINEABLE _CachePolicyInUse #-}
{-# DEPRECATED _CachePolicyInUse "Use generic-lens or generic-optics instead"  #-}

-- | The specified invalidation does not exist.
_NoSuchInvalidation :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchInvalidation
  = Core._MatchServiceError mkServiceConfig "NoSuchInvalidation"
      Core.. Core.hasStatues 404
{-# INLINEABLE _NoSuchInvalidation #-}
{-# DEPRECATED _NoSuchInvalidation "Use generic-lens or generic-optics instead"  #-}

-- | The specified public key already exists.
_PublicKeyAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PublicKeyAlreadyExists
  = Core._MatchServiceError mkServiceConfig "PublicKeyAlreadyExists"
      Core.. Core.hasStatues 409
{-# INLINEABLE _PublicKeyAlreadyExists #-}
{-# DEPRECATED _PublicKeyAlreadyExists "Use generic-lens or generic-optics instead"  #-}

-- | No origin exists with the specified @Origin Id@ . 
_NoSuchOrigin :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchOrigin
  = Core._MatchServiceError mkServiceConfig "NoSuchOrigin" Core..
      Core.hasStatues 404
{-# INLINEABLE _NoSuchOrigin #-}
{-# DEPRECATED _NoSuchOrigin "Use generic-lens or generic-optics instead"  #-}

-- | The number of headers in the cache policy exceeds the maximum. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas> (formerly known as limits) in the /Amazon CloudFront Developer Guide/ .
_TooManyHeadersInCachePolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyHeadersInCachePolicy
  = Core._MatchServiceError mkServiceConfig
      "TooManyHeadersInCachePolicy"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyHeadersInCachePolicy #-}
{-# DEPRECATED _TooManyHeadersInCachePolicy "Use generic-lens or generic-optics instead"  #-}

-- | The cache policy does not exist.
_NoSuchCachePolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchCachePolicy
  = Core._MatchServiceError mkServiceConfig "NoSuchCachePolicy"
      Core.. Core.hasStatues 404
{-# INLINEABLE _NoSuchCachePolicy #-}
{-# DEPRECATED _NoSuchCachePolicy "Use generic-lens or generic-optics instead"  #-}

-- | The TTL order specified is not valid.
_InvalidTTLOrder :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTTLOrder
  = Core._MatchServiceError mkServiceConfig "InvalidTTLOrder" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidTTLOrder #-}
{-# DEPRECATED _InvalidTTLOrder "Use generic-lens or generic-optics instead"  #-}

-- | The specified CloudFront distribution is not disabled. You must disable the distribution before you can delete it.
_StreamingDistributionNotDisabled :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StreamingDistributionNotDisabled
  = Core._MatchServiceError mkServiceConfig
      "StreamingDistributionNotDisabled"
      Core.. Core.hasStatues 409
{-# INLINEABLE _StreamingDistributionNotDisabled #-}
{-# DEPRECATED _StreamingDistributionNotDisabled "Use generic-lens or generic-optics instead"  #-}

-- | An origin request policy with this name already exists. You must provide a unique name. To modify an existing origin request policy, use @UpdateOriginRequestPolicy@ .
_OriginRequestPolicyAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OriginRequestPolicyAlreadyExists
  = Core._MatchServiceError mkServiceConfig
      "OriginRequestPolicyAlreadyExists"
      Core.. Core.hasStatues 409
{-# INLINEABLE _OriginRequestPolicyAlreadyExists #-}
{-# DEPRECATED _OriginRequestPolicyAlreadyExists "Use generic-lens or generic-optics instead"  #-}

-- | Your request contains too many headers in forwarded values.
_TooManyHeadersInForwardedValues :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyHeadersInForwardedValues
  = Core._MatchServiceError mkServiceConfig
      "TooManyHeadersInForwardedValues"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyHeadersInForwardedValues #-}
{-# DEPRECATED _TooManyHeadersInForwardedValues "Use generic-lens or generic-optics instead"  #-}

-- | A resource that was specified is not valid.
_NoSuchResource :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchResource
  = Core._MatchServiceError mkServiceConfig "NoSuchResource" Core..
      Core.hasStatues 404
{-# INLINEABLE _NoSuchResource #-}
{-# DEPRECATED _NoSuchResource "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of encryption entities for field-level encryption have been created.
_TooManyFieldLevelEncryptionEncryptionEntities :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyFieldLevelEncryptionEncryptionEntities
  = Core._MatchServiceError mkServiceConfig
      "TooManyFieldLevelEncryptionEncryptionEntities"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyFieldLevelEncryptionEncryptionEntities #-}
{-# DEPRECATED _TooManyFieldLevelEncryptionEncryptionEntities "Use generic-lens or generic-optics instead"  #-}

-- | Your request contains more CNAMEs than are allowed per distribution.
_TooManyStreamingDistributionCNAMEs :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyStreamingDistributionCNAMEs
  = Core._MatchServiceError mkServiceConfig
      "TooManyStreamingDistributionCNAMEs"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyStreamingDistributionCNAMEs #-}
{-# DEPRECATED _TooManyStreamingDistributionCNAMEs "Use generic-lens or generic-optics instead"  #-}

-- | The specified profile for field-level encryption already exists.
_FieldLevelEncryptionProfileAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FieldLevelEncryptionProfileAlreadyExists
  = Core._MatchServiceError mkServiceConfig
      "FieldLevelEncryptionProfileAlreadyExists"
      Core.. Core.hasStatues 409
{-# INLINEABLE _FieldLevelEncryptionProfileAlreadyExists #-}
{-# DEPRECATED _FieldLevelEncryptionProfileAlreadyExists "Use generic-lens or generic-optics instead"  #-}

-- | A key group with this name already exists. You must provide a unique name. To modify an existing key group, use @UpdateKeyGroup@ .
_KeyGroupAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KeyGroupAlreadyExists
  = Core._MatchServiceError mkServiceConfig "KeyGroupAlreadyExists"
      Core.. Core.hasStatues 409
{-# INLINEABLE _KeyGroupAlreadyExists #-}
{-# DEPRECATED _KeyGroupAlreadyExists "Use generic-lens or generic-optics instead"  #-}

-- | The specified key group does not exist.
_TrustedKeyGroupDoesNotExist :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TrustedKeyGroupDoesNotExist
  = Core._MatchServiceError mkServiceConfig
      "TrustedKeyGroupDoesNotExist"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TrustedKeyGroupDoesNotExist #-}
{-# DEPRECATED _TrustedKeyGroupDoesNotExist "Use generic-lens or generic-optics instead"  #-}

-- | Cannot delete this resource because it is in use.
_ResourceInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUse
  = Core._MatchServiceError mkServiceConfig "ResourceInUse" Core..
      Core.hasStatues 409
{-# INLINEABLE _ResourceInUse #-}
{-# DEPRECATED _ResourceInUse "Use generic-lens or generic-optics instead"  #-}

-- | This operation requires the HTTPS protocol. Ensure that you specify the HTTPS protocol in your request, or omit the @RequiredProtocols@ element from your distribution configuration.
_InvalidRequiredProtocol :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequiredProtocol
  = Core._MatchServiceError mkServiceConfig "InvalidRequiredProtocol"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidRequiredProtocol #-}
{-# DEPRECATED _InvalidRequiredProtocol "Use generic-lens or generic-optics instead"  #-}

-- | Processing your request would cause you to exceed the maximum number of distributions allowed.
_TooManyDistributions :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyDistributions
  = Core._MatchServiceError mkServiceConfig "TooManyDistributions"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyDistributions #-}
{-# DEPRECATED _TooManyDistributions "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of distributions have been associated with the specified Lambda function.
_TooManyDistributionsWithSingleFunctionARN :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyDistributionsWithSingleFunctionARN
  = Core._MatchServiceError mkServiceConfig
      "TooManyDistributionsWithSingleFunctionARN"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyDistributionsWithSingleFunctionARN #-}
{-# DEPRECATED _TooManyDistributionsWithSingleFunctionARN "Use generic-lens or generic-optics instead"  #-}

-- | The number of headers in the origin request policy exceeds the maximum. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas> (formerly known as limits) in the /Amazon CloudFront Developer Guide/ .
_TooManyHeadersInOriginRequestPolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyHeadersInOriginRequestPolicy
  = Core._MatchServiceError mkServiceConfig
      "TooManyHeadersInOriginRequestPolicy"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyHeadersInOriginRequestPolicy #-}
{-# DEPRECATED _TooManyHeadersInOriginRequestPolicy "Use generic-lens or generic-optics instead"  #-}

-- | You cannot create anymore custom SSL/TLS certificates.
_TooManyCertificates :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyCertificates
  = Core._MatchServiceError mkServiceConfig "TooManyCertificates"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyCertificates #-}
{-# DEPRECATED _TooManyCertificates "Use generic-lens or generic-optics instead"  #-}

-- | The origin request policy does not exist.
_NoSuchOriginRequestPolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchOriginRequestPolicy
  = Core._MatchServiceError mkServiceConfig
      "NoSuchOriginRequestPolicy"
      Core.. Core.hasStatues 404
{-# INLINEABLE _NoSuchOriginRequestPolicy #-}
{-# DEPRECATED _NoSuchOriginRequestPolicy "Use generic-lens or generic-optics instead"  #-}

-- | The caller reference you attempted to create the distribution with is associated with another distribution.
_DistributionAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DistributionAlreadyExists
  = Core._MatchServiceError mkServiceConfig
      "DistributionAlreadyExists"
      Core.. Core.hasStatues 409
{-# INLINEABLE _DistributionAlreadyExists #-}
{-# DEPRECATED _DistributionAlreadyExists "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of distributions have been associated with the specified configuration for field-level encryption.
_TooManyDistributionsAssociatedToFieldLevelEncryptionConfig :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyDistributionsAssociatedToFieldLevelEncryptionConfig
  = Core._MatchServiceError mkServiceConfig
      "TooManyDistributionsAssociatedToFieldLevelEncryptionConfig"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyDistributionsAssociatedToFieldLevelEncryptionConfig #-}
{-# DEPRECATED _TooManyDistributionsAssociatedToFieldLevelEncryptionConfig "Use generic-lens or generic-optics instead"  #-}

-- | You have reached the maximum number of key groups for this AWS account. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas> (formerly known as limits) in the /Amazon CloudFront Developer Guide/ .
_TooManyKeyGroups :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyKeyGroups
  = Core._MatchServiceError mkServiceConfig "TooManyKeyGroups" Core..
      Core.hasStatues 400
{-# INLINEABLE _TooManyKeyGroups #-}
{-# DEPRECATED _TooManyKeyGroups "Use generic-lens or generic-optics instead"  #-}

-- | The query string parameters specified are not valid.
_InvalidQueryStringParameters :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidQueryStringParameters
  = Core._MatchServiceError mkServiceConfig
      "InvalidQueryStringParameters"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidQueryStringParameters #-}
{-# DEPRECATED _InvalidQueryStringParameters "Use generic-lens or generic-optics instead"  #-}

-- | This operation requires a body. Ensure that the body is present and the @Content-Type@ header is set.
_MissingBody :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MissingBody
  = Core._MatchServiceError mkServiceConfig "MissingBody" Core..
      Core.hasStatues 400
{-# INLINEABLE _MissingBody #-}
{-# DEPRECATED _MissingBody "Use generic-lens or generic-optics instead"  #-}

-- | The specified configuration for field-level encryption can't be associated with the specified cache behavior.
_IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior
  = Core._MatchServiceError mkServiceConfig
      "IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior"
      Core.. Core.hasStatues 400
{-# INLINEABLE _IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior #-}
{-# DEPRECATED _IllegalFieldLevelEncryptionConfigAssociationWithCacheBehavior "Use generic-lens or generic-optics instead"  #-}

-- | You have reached the maximum number of origin request policies for this AWS account. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas> (formerly known as limits) in the /Amazon CloudFront Developer Guide/ .
_TooManyOriginRequestPolicies :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyOriginRequestPolicies
  = Core._MatchServiceError mkServiceConfig
      "TooManyOriginRequestPolicies"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyOriginRequestPolicies #-}
{-# DEPRECATED _TooManyOriginRequestPolicies "Use generic-lens or generic-optics instead"  #-}

-- | You cannot delete a managed policy.
_IllegalDelete :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IllegalDelete
  = Core._MatchServiceError mkServiceConfig "IllegalDelete" Core..
      Core.hasStatues 400
{-# INLINEABLE _IllegalDelete #-}
{-# DEPRECATED _IllegalDelete "Use generic-lens or generic-optics instead"  #-}

-- | The update contains modifications that are not allowed.
_IllegalUpdate :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IllegalUpdate
  = Core._MatchServiceError mkServiceConfig "IllegalUpdate" Core..
      Core.hasStatues 400
{-# INLINEABLE _IllegalUpdate #-}
{-# DEPRECATED _IllegalUpdate "Use generic-lens or generic-optics instead"  #-}

-- | The @If-Match@ version is missing or not valid.
_InvalidIfMatchVersion :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidIfMatchVersion
  = Core._MatchServiceError mkServiceConfig "InvalidIfMatchVersion"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidIfMatchVersion #-}
{-# DEPRECATED _InvalidIfMatchVersion "Use generic-lens or generic-optics instead"  #-}

-- | The specified configuration for field-level encryption already exists.
_FieldLevelEncryptionConfigAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FieldLevelEncryptionConfigAlreadyExists
  = Core._MatchServiceError mkServiceConfig
      "FieldLevelEncryptionConfigAlreadyExists"
      Core.. Core.hasStatues 409
{-# INLINEABLE _FieldLevelEncryptionConfigAlreadyExists #-}
{-# DEPRECATED _FieldLevelEncryptionConfigAlreadyExists "Use generic-lens or generic-optics instead"  #-}

-- | The precondition given in one or more of the request header fields evaluated to @false@ .
_PreconditionFailed :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PreconditionFailed
  = Core._MatchServiceError mkServiceConfig "PreconditionFailed"
      Core.. Core.hasStatues 412
{-# INLINEABLE _PreconditionFailed #-}
{-# DEPRECATED _PreconditionFailed "Use generic-lens or generic-optics instead"  #-}

-- | A response code is not valid.
_InvalidResponseCode :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidResponseCode
  = Core._MatchServiceError mkServiceConfig "InvalidResponseCode"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidResponseCode #-}
{-# DEPRECATED _InvalidResponseCode "Use generic-lens or generic-optics instead"  #-}

-- | The headers specified are not valid for an Amazon S3 origin.
_InvalidHeadersForS3Origin :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidHeadersForS3Origin
  = Core._MatchServiceError mkServiceConfig
      "InvalidHeadersForS3Origin"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidHeadersForS3Origin #-}
{-# DEPRECATED _InvalidHeadersForS3Origin "Use generic-lens or generic-optics instead"  #-}

-- | The CNAME specified is already defined for CloudFront.
_CNAMEAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CNAMEAlreadyExists
  = Core._MatchServiceError mkServiceConfig "CNAMEAlreadyExists"
      Core.. Core.hasStatues 409
{-# INLINEABLE _CNAMEAlreadyExists #-}
{-# DEPRECATED _CNAMEAlreadyExists "Use generic-lens or generic-optics instead"  #-}

-- | The specified public key doesn't exist.
_NoSuchPublicKey :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchPublicKey
  = Core._MatchServiceError mkServiceConfig "NoSuchPublicKey" Core..
      Core.hasStatues 404
{-# INLINEABLE _NoSuchPublicKey #-}
{-# DEPRECATED _NoSuchPublicKey "Use generic-lens or generic-optics instead"  #-}

-- | The specified public key is in use. 
_PublicKeyInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PublicKeyInUse
  = Core._MatchServiceError mkServiceConfig "PublicKeyInUse" Core..
      Core.hasStatues 409
{-# INLINEABLE _PublicKeyInUse #-}
{-# DEPRECATED _PublicKeyInUse "Use generic-lens or generic-optics instead"  #-}

-- | One or more of your trusted signers don't exist.
_TrustedSignerDoesNotExist :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TrustedSignerDoesNotExist
  = Core._MatchServiceError mkServiceConfig
      "TrustedSignerDoesNotExist"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TrustedSignerDoesNotExist #-}
{-# DEPRECATED _TrustedSignerDoesNotExist "Use generic-lens or generic-optics instead"  #-}

-- | You cannot specify SSLv3 as the minimum protocol version if you only want to support only clients that support Server Name Indication (SNI).
_InvalidProtocolSettings :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidProtocolSettings
  = Core._MatchServiceError mkServiceConfig "InvalidProtocolSettings"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidProtocolSettings #-}
{-# DEPRECATED _InvalidProtocolSettings "Use generic-lens or generic-optics instead"  #-}

-- | A cache policy with this name already exists. You must provide a unique name. To modify an existing cache policy, use @UpdateCachePolicy@ .
_CachePolicyAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CachePolicyAlreadyExists
  = Core._MatchServiceError mkServiceConfig
      "CachePolicyAlreadyExists"
      Core.. Core.hasStatues 409
{-# INLINEABLE _CachePolicyAlreadyExists #-}
{-# DEPRECATED _CachePolicyAlreadyExists "Use generic-lens or generic-optics instead"  #-}

-- | The number of cookies in the origin request policy exceeds the maximum. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas> (formerly known as limits) in the /Amazon CloudFront Developer Guide/ .
_TooManyCookiesInOriginRequestPolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyCookiesInOriginRequestPolicy
  = Core._MatchServiceError mkServiceConfig
      "TooManyCookiesInOriginRequestPolicy"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyCookiesInOriginRequestPolicy #-}
{-# DEPRECATED _TooManyCookiesInOriginRequestPolicy "Use generic-lens or generic-optics instead"  #-}

-- | Processing your request would cause you to exceed the maximum number of origin groups allowed.
_TooManyOriginGroupsPerDistribution :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyOriginGroupsPerDistribution
  = Core._MatchServiceError mkServiceConfig
      "TooManyOriginGroupsPerDistribution"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyOriginGroupsPerDistribution #-}
{-# DEPRECATED _TooManyOriginGroupsPerDistribution "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of public keys for field-level encryption have been created. To create a new public key, delete one of the existing keys.
_TooManyPublicKeys :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyPublicKeys
  = Core._MatchServiceError mkServiceConfig "TooManyPublicKeys"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyPublicKeys #-}
{-# DEPRECATED _TooManyPublicKeys "Use generic-lens or generic-optics instead"  #-}

-- | The specified configuration for field-level encryption doesn't exist.
_NoSuchFieldLevelEncryptionConfig :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchFieldLevelEncryptionConfig
  = Core._MatchServiceError mkServiceConfig
      "NoSuchFieldLevelEncryptionConfig"
      Core.. Core.hasStatues 404
{-# INLINEABLE _NoSuchFieldLevelEncryptionConfig #-}
{-# DEPRECATED _NoSuchFieldLevelEncryptionConfig "Use generic-lens or generic-optics instead"  #-}

-- | You have reached the maximum number of real-time log configurations for this AWS account. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas> (formerly known as limits) in the /Amazon CloudFront Developer Guide/ .
_TooManyRealtimeLogConfigs :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRealtimeLogConfigs
  = Core._MatchServiceError mkServiceConfig
      "TooManyRealtimeLogConfigs"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyRealtimeLogConfigs #-}
{-# DEPRECATED _TooManyRealtimeLogConfigs "Use generic-lens or generic-optics instead"  #-}

-- | Cannot delete the real-time log configuration because it is attached to one or more cache behaviors.
_RealtimeLogConfigInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RealtimeLogConfigInUse
  = Core._MatchServiceError mkServiceConfig "RealtimeLogConfigInUse"
      Core.. Core.hasStatues 400
{-# INLINEABLE _RealtimeLogConfigInUse #-}
{-# DEPRECATED _RealtimeLogConfigInUse "Use generic-lens or generic-optics instead"  #-}

-- | You have reached the maximum number of cache policies for this AWS account. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas> (formerly known as limits) in the /Amazon CloudFront Developer Guide/ .
_TooManyCachePolicies :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyCachePolicies
  = Core._MatchServiceError mkServiceConfig "TooManyCachePolicies"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyCachePolicies #-}
{-# DEPRECATED _TooManyCachePolicies "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of content type profiles for field-level encryption have been created.
_TooManyFieldLevelEncryptionContentTypeProfiles :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyFieldLevelEncryptionContentTypeProfiles
  = Core._MatchServiceError mkServiceConfig
      "TooManyFieldLevelEncryptionContentTypeProfiles"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyFieldLevelEncryptionContentTypeProfiles #-}
{-# DEPRECATED _TooManyFieldLevelEncryptionContentTypeProfiles "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of field patterns for field-level encryption have been created.
_TooManyFieldLevelEncryptionFieldPatterns :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyFieldLevelEncryptionFieldPatterns
  = Core._MatchServiceError mkServiceConfig
      "TooManyFieldLevelEncryptionFieldPatterns"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyFieldLevelEncryptionFieldPatterns #-}
{-# DEPRECATED _TooManyFieldLevelEncryptionFieldPatterns "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of configurations for field-level encryption have been created.
_TooManyFieldLevelEncryptionConfigs :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyFieldLevelEncryptionConfigs
  = Core._MatchServiceError mkServiceConfig
      "TooManyFieldLevelEncryptionConfigs"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyFieldLevelEncryptionConfigs #-}
{-# DEPRECATED _TooManyFieldLevelEncryptionConfigs "Use generic-lens or generic-optics instead"  #-}

-- | Your request contains more Lambda function associations than are allowed per distribution.
_TooManyLambdaFunctionAssociations :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyLambdaFunctionAssociations
  = Core._MatchServiceError mkServiceConfig
      "TooManyLambdaFunctionAssociations"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyLambdaFunctionAssociations #-}
{-# DEPRECATED _TooManyLambdaFunctionAssociations "Use generic-lens or generic-optics instead"  #-}

-- | If the @CallerReference@ is a value you already sent in a previous request to create an identity but the content of the @CloudFrontOriginAccessIdentityConfig@ is different from the original request, CloudFront returns a @CloudFrontOriginAccessIdentityAlreadyExists@ error. 
_CloudFrontOriginAccessIdentityAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudFrontOriginAccessIdentityAlreadyExists
  = Core._MatchServiceError mkServiceConfig
      "CloudFrontOriginAccessIdentityAlreadyExists"
      Core.. Core.hasStatues 409
{-# INLINEABLE _CloudFrontOriginAccessIdentityAlreadyExists #-}
{-# DEPRECATED _CloudFrontOriginAccessIdentityAlreadyExists "Use generic-lens or generic-optics instead"  #-}

-- | The number of query strings in the cache policy exceeds the maximum. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas> (formerly known as limits) in the /Amazon CloudFront Developer Guide/ .
_TooManyQueryStringsInCachePolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyQueryStringsInCachePolicy
  = Core._MatchServiceError mkServiceConfig
      "TooManyQueryStringsInCachePolicy"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyQueryStringsInCachePolicy #-}
{-# DEPRECATED _TooManyQueryStringsInCachePolicy "Use generic-lens or generic-optics instead"  #-}

-- | You cannot create more origins for the distribution.
_TooManyOrigins :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyOrigins
  = Core._MatchServiceError mkServiceConfig "TooManyOrigins" Core..
      Core.hasStatues 400
{-# INLINEABLE _TooManyOrigins #-}
{-# DEPRECATED _TooManyOrigins "Use generic-lens or generic-optics instead"  #-}

-- | The relative path is too big, is not URL-encoded, or does not begin with a slash (/).
_InvalidRelativePath :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRelativePath
  = Core._MatchServiceError mkServiceConfig "InvalidRelativePath"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidRelativePath #-}
{-# DEPRECATED _InvalidRelativePath "Use generic-lens or generic-optics instead"  #-}

-- | The caller reference you attempted to create the streaming distribution with is associated with another distribution
_StreamingDistributionAlreadyExists :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StreamingDistributionAlreadyExists
  = Core._MatchServiceError mkServiceConfig
      "StreamingDistributionAlreadyExists"
      Core.. Core.hasStatues 409
{-# INLINEABLE _StreamingDistributionAlreadyExists #-}
{-# DEPRECATED _StreamingDistributionAlreadyExists "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of distributions have been associated with the specified origin request policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas> (formerly known as limits) in the /Amazon CloudFront Developer Guide/ .
_TooManyDistributionsAssociatedToOriginRequestPolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyDistributionsAssociatedToOriginRequestPolicy
  = Core._MatchServiceError mkServiceConfig
      "TooManyDistributionsAssociatedToOriginRequestPolicy"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyDistributionsAssociatedToOriginRequestPolicy #-}
{-# DEPRECATED _TooManyDistributionsAssociatedToOriginRequestPolicy "Use generic-lens or generic-optics instead"  #-}

-- | No profile specified for the field-level encryption query argument.
_QueryArgProfileEmpty :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_QueryArgProfileEmpty
  = Core._MatchServiceError mkServiceConfig "QueryArgProfileEmpty"
      Core.. Core.hasStatues 400
{-# INLINEABLE _QueryArgProfileEmpty #-}
{-# DEPRECATED _QueryArgProfileEmpty "Use generic-lens or generic-optics instead"  #-}

-- | The number of cookies in the cache policy exceeds the maximum. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas> (formerly known as limits) in the /Amazon CloudFront Developer Guide/ .
_TooManyCookiesInCachePolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyCookiesInCachePolicy
  = Core._MatchServiceError mkServiceConfig
      "TooManyCookiesInCachePolicy"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyCookiesInCachePolicy #-}
{-# DEPRECATED _TooManyCookiesInCachePolicy "Use generic-lens or generic-optics instead"  #-}

-- | The minimum protocol version specified is not valid.
_InvalidMinimumProtocolVersion :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidMinimumProtocolVersion
  = Core._MatchServiceError mkServiceConfig
      "InvalidMinimumProtocolVersion"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidMinimumProtocolVersion #-}
{-# DEPRECATED _InvalidMinimumProtocolVersion "Use generic-lens or generic-optics instead"  #-}

-- | Access denied.
_AccessDenied :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDenied
  = Core._MatchServiceError mkServiceConfig "AccessDenied" Core..
      Core.hasStatues 403
{-# INLINEABLE _AccessDenied #-}
{-# DEPRECATED _AccessDenied "Use generic-lens or generic-optics instead"  #-}

-- | A viewer certificate specified is not valid.
_InvalidViewerCertificate :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidViewerCertificate
  = Core._MatchServiceError mkServiceConfig
      "InvalidViewerCertificate"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidViewerCertificate #-}
{-# DEPRECATED _InvalidViewerCertificate "Use generic-lens or generic-optics instead"  #-}

-- | The specified distribution does not exist.
_NoSuchDistribution :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchDistribution
  = Core._MatchServiceError mkServiceConfig "NoSuchDistribution"
      Core.. Core.hasStatues 404
{-# INLINEABLE _NoSuchDistribution #-}
{-# DEPRECATED _NoSuchDistribution "Use generic-lens or generic-optics instead"  #-}

-- | The maximum size of a profile for field-level encryption was exceeded.
_FieldLevelEncryptionProfileSizeExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FieldLevelEncryptionProfileSizeExceeded
  = Core._MatchServiceError mkServiceConfig
      "FieldLevelEncryptionProfileSizeExceeded"
      Core.. Core.hasStatues 400
{-# INLINEABLE _FieldLevelEncryptionProfileSizeExceeded #-}
{-# DEPRECATED _FieldLevelEncryptionProfileSizeExceeded "Use generic-lens or generic-optics instead"  #-}

-- | The number of query strings in the origin request policy exceeds the maximum. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas> (formerly known as limits) in the /Amazon CloudFront Developer Guide/ .
_TooManyQueryStringsInOriginRequestPolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyQueryStringsInOriginRequestPolicy
  = Core._MatchServiceError mkServiceConfig
      "TooManyQueryStringsInOriginRequestPolicy"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyQueryStringsInOriginRequestPolicy #-}
{-# DEPRECATED _TooManyQueryStringsInOriginRequestPolicy "Use generic-lens or generic-optics instead"  #-}

-- | The default root object file name is too big or contains an invalid character.
_InvalidDefaultRootObject :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDefaultRootObject
  = Core._MatchServiceError mkServiceConfig
      "InvalidDefaultRootObject"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidDefaultRootObject #-}
{-# DEPRECATED _InvalidDefaultRootObject "Use generic-lens or generic-optics instead"  #-}

-- | Processing your request would cause the maximum number of distributions with Lambda function associations per owner to be exceeded.
_TooManyDistributionsWithLambdaAssociations :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyDistributionsWithLambdaAssociations
  = Core._MatchServiceError mkServiceConfig
      "TooManyDistributionsWithLambdaAssociations"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyDistributionsWithLambdaAssociations #-}
{-# DEPRECATED _TooManyDistributionsWithLambdaAssociations "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of distributions have been associated with the specified cache policy. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html Quotas> (formerly known as limits) in the /Amazon CloudFront Developer Guide/ .
_TooManyDistributionsAssociatedToCachePolicy :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyDistributionsAssociatedToCachePolicy
  = Core._MatchServiceError mkServiceConfig
      "TooManyDistributionsAssociatedToCachePolicy"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyDistributionsAssociatedToCachePolicy #-}
{-# DEPRECATED _TooManyDistributionsAssociatedToCachePolicy "Use generic-lens or generic-optics instead"  #-}

-- | The specified geo restriction parameter is not valid.
_InvalidGeoRestrictionParameter :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidGeoRestrictionParameter
  = Core._MatchServiceError mkServiceConfig
      "InvalidGeoRestrictionParameter"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidGeoRestrictionParameter #-}
{-# DEPRECATED _InvalidGeoRestrictionParameter "Use generic-lens or generic-optics instead"  #-}

-- | Cannot delete the origin request policy because it is attached to one or more cache behaviors.
_OriginRequestPolicyInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OriginRequestPolicyInUse
  = Core._MatchServiceError mkServiceConfig
      "OriginRequestPolicyInUse"
      Core.. Core.hasStatues 409
{-# INLINEABLE _OriginRequestPolicyInUse #-}
{-# DEPRECATED _OriginRequestPolicyInUse "Use generic-lens or generic-optics instead"  #-}

-- | The location code specified is not valid.
_InvalidLocationCode :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLocationCode
  = Core._MatchServiceError mkServiceConfig "InvalidLocationCode"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidLocationCode #-}
{-# DEPRECATED _InvalidLocationCode "Use generic-lens or generic-optics instead"  #-}
