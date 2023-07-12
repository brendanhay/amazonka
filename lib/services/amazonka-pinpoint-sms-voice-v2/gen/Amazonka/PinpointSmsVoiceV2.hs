{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.PinpointSmsVoiceV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2022-03-31@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Welcome to the /Amazon Pinpoint SMS and Voice, version 2 API Reference/.
-- This guide provides information about Amazon Pinpoint SMS and Voice,
-- version 2 API resources, including supported HTTP methods, parameters,
-- and schemas.
--
-- Amazon Pinpoint is an Amazon Web Services service that you can use to
-- engage with your recipients across multiple messaging channels. The
-- Amazon Pinpoint SMS and Voice, version 2 API provides programmatic
-- access to options that are unique to the SMS and voice channels and
-- supplements the resources provided by the Amazon Pinpoint API.
--
-- If you\'re new to Amazon Pinpoint, it\'s also helpful to review the
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/welcome.html Amazon Pinpoint Developer Guide>.
-- The /Amazon Pinpoint Developer Guide/ provides tutorials, code samples,
-- and procedures that demonstrate how to use Amazon Pinpoint features
-- programmatically and how to integrate Amazon Pinpoint functionality into
-- mobile apps and other types of applications. The guide also provides key
-- information, such as Amazon Pinpoint integration with other Amazon Web
-- Services services, and the quotas that apply to use of the service.
module Amazonka.PinpointSmsVoiceV2
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateOriginationIdentity
    AssociateOriginationIdentity (AssociateOriginationIdentity'),
    newAssociateOriginationIdentity,
    AssociateOriginationIdentityResponse (AssociateOriginationIdentityResponse'),
    newAssociateOriginationIdentityResponse,

    -- ** CreateConfigurationSet
    CreateConfigurationSet (CreateConfigurationSet'),
    newCreateConfigurationSet,
    CreateConfigurationSetResponse (CreateConfigurationSetResponse'),
    newCreateConfigurationSetResponse,

    -- ** CreateEventDestination
    CreateEventDestination (CreateEventDestination'),
    newCreateEventDestination,
    CreateEventDestinationResponse (CreateEventDestinationResponse'),
    newCreateEventDestinationResponse,

    -- ** CreateOptOutList
    CreateOptOutList (CreateOptOutList'),
    newCreateOptOutList,
    CreateOptOutListResponse (CreateOptOutListResponse'),
    newCreateOptOutListResponse,

    -- ** CreatePool
    CreatePool (CreatePool'),
    newCreatePool,
    CreatePoolResponse (CreatePoolResponse'),
    newCreatePoolResponse,

    -- ** DeleteConfigurationSet
    DeleteConfigurationSet (DeleteConfigurationSet'),
    newDeleteConfigurationSet,
    DeleteConfigurationSetResponse (DeleteConfigurationSetResponse'),
    newDeleteConfigurationSetResponse,

    -- ** DeleteDefaultMessageType
    DeleteDefaultMessageType (DeleteDefaultMessageType'),
    newDeleteDefaultMessageType,
    DeleteDefaultMessageTypeResponse (DeleteDefaultMessageTypeResponse'),
    newDeleteDefaultMessageTypeResponse,

    -- ** DeleteDefaultSenderId
    DeleteDefaultSenderId (DeleteDefaultSenderId'),
    newDeleteDefaultSenderId,
    DeleteDefaultSenderIdResponse (DeleteDefaultSenderIdResponse'),
    newDeleteDefaultSenderIdResponse,

    -- ** DeleteEventDestination
    DeleteEventDestination (DeleteEventDestination'),
    newDeleteEventDestination,
    DeleteEventDestinationResponse (DeleteEventDestinationResponse'),
    newDeleteEventDestinationResponse,

    -- ** DeleteKeyword
    DeleteKeyword (DeleteKeyword'),
    newDeleteKeyword,
    DeleteKeywordResponse (DeleteKeywordResponse'),
    newDeleteKeywordResponse,

    -- ** DeleteOptOutList
    DeleteOptOutList (DeleteOptOutList'),
    newDeleteOptOutList,
    DeleteOptOutListResponse (DeleteOptOutListResponse'),
    newDeleteOptOutListResponse,

    -- ** DeleteOptedOutNumber
    DeleteOptedOutNumber (DeleteOptedOutNumber'),
    newDeleteOptedOutNumber,
    DeleteOptedOutNumberResponse (DeleteOptedOutNumberResponse'),
    newDeleteOptedOutNumberResponse,

    -- ** DeletePool
    DeletePool (DeletePool'),
    newDeletePool,
    DeletePoolResponse (DeletePoolResponse'),
    newDeletePoolResponse,

    -- ** DeleteTextMessageSpendLimitOverride
    DeleteTextMessageSpendLimitOverride (DeleteTextMessageSpendLimitOverride'),
    newDeleteTextMessageSpendLimitOverride,
    DeleteTextMessageSpendLimitOverrideResponse (DeleteTextMessageSpendLimitOverrideResponse'),
    newDeleteTextMessageSpendLimitOverrideResponse,

    -- ** DeleteVoiceMessageSpendLimitOverride
    DeleteVoiceMessageSpendLimitOverride (DeleteVoiceMessageSpendLimitOverride'),
    newDeleteVoiceMessageSpendLimitOverride,
    DeleteVoiceMessageSpendLimitOverrideResponse (DeleteVoiceMessageSpendLimitOverrideResponse'),
    newDeleteVoiceMessageSpendLimitOverrideResponse,

    -- ** DescribeAccountAttributes (Paginated)
    DescribeAccountAttributes (DescribeAccountAttributes'),
    newDescribeAccountAttributes,
    DescribeAccountAttributesResponse (DescribeAccountAttributesResponse'),
    newDescribeAccountAttributesResponse,

    -- ** DescribeAccountLimits (Paginated)
    DescribeAccountLimits (DescribeAccountLimits'),
    newDescribeAccountLimits,
    DescribeAccountLimitsResponse (DescribeAccountLimitsResponse'),
    newDescribeAccountLimitsResponse,

    -- ** DescribeConfigurationSets (Paginated)
    DescribeConfigurationSets (DescribeConfigurationSets'),
    newDescribeConfigurationSets,
    DescribeConfigurationSetsResponse (DescribeConfigurationSetsResponse'),
    newDescribeConfigurationSetsResponse,

    -- ** DescribeKeywords (Paginated)
    DescribeKeywords (DescribeKeywords'),
    newDescribeKeywords,
    DescribeKeywordsResponse (DescribeKeywordsResponse'),
    newDescribeKeywordsResponse,

    -- ** DescribeOptOutLists (Paginated)
    DescribeOptOutLists (DescribeOptOutLists'),
    newDescribeOptOutLists,
    DescribeOptOutListsResponse (DescribeOptOutListsResponse'),
    newDescribeOptOutListsResponse,

    -- ** DescribeOptedOutNumbers (Paginated)
    DescribeOptedOutNumbers (DescribeOptedOutNumbers'),
    newDescribeOptedOutNumbers,
    DescribeOptedOutNumbersResponse (DescribeOptedOutNumbersResponse'),
    newDescribeOptedOutNumbersResponse,

    -- ** DescribePhoneNumbers (Paginated)
    DescribePhoneNumbers (DescribePhoneNumbers'),
    newDescribePhoneNumbers,
    DescribePhoneNumbersResponse (DescribePhoneNumbersResponse'),
    newDescribePhoneNumbersResponse,

    -- ** DescribePools (Paginated)
    DescribePools (DescribePools'),
    newDescribePools,
    DescribePoolsResponse (DescribePoolsResponse'),
    newDescribePoolsResponse,

    -- ** DescribeSenderIds (Paginated)
    DescribeSenderIds (DescribeSenderIds'),
    newDescribeSenderIds,
    DescribeSenderIdsResponse (DescribeSenderIdsResponse'),
    newDescribeSenderIdsResponse,

    -- ** DescribeSpendLimits (Paginated)
    DescribeSpendLimits (DescribeSpendLimits'),
    newDescribeSpendLimits,
    DescribeSpendLimitsResponse (DescribeSpendLimitsResponse'),
    newDescribeSpendLimitsResponse,

    -- ** DisassociateOriginationIdentity
    DisassociateOriginationIdentity (DisassociateOriginationIdentity'),
    newDisassociateOriginationIdentity,
    DisassociateOriginationIdentityResponse (DisassociateOriginationIdentityResponse'),
    newDisassociateOriginationIdentityResponse,

    -- ** ListPoolOriginationIdentities (Paginated)
    ListPoolOriginationIdentities (ListPoolOriginationIdentities'),
    newListPoolOriginationIdentities,
    ListPoolOriginationIdentitiesResponse (ListPoolOriginationIdentitiesResponse'),
    newListPoolOriginationIdentitiesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutKeyword
    PutKeyword (PutKeyword'),
    newPutKeyword,
    PutKeywordResponse (PutKeywordResponse'),
    newPutKeywordResponse,

    -- ** PutOptedOutNumber
    PutOptedOutNumber (PutOptedOutNumber'),
    newPutOptedOutNumber,
    PutOptedOutNumberResponse (PutOptedOutNumberResponse'),
    newPutOptedOutNumberResponse,

    -- ** ReleasePhoneNumber
    ReleasePhoneNumber (ReleasePhoneNumber'),
    newReleasePhoneNumber,
    ReleasePhoneNumberResponse (ReleasePhoneNumberResponse'),
    newReleasePhoneNumberResponse,

    -- ** RequestPhoneNumber
    RequestPhoneNumber (RequestPhoneNumber'),
    newRequestPhoneNumber,
    RequestPhoneNumberResponse (RequestPhoneNumberResponse'),
    newRequestPhoneNumberResponse,

    -- ** SendTextMessage
    SendTextMessage (SendTextMessage'),
    newSendTextMessage,
    SendTextMessageResponse (SendTextMessageResponse'),
    newSendTextMessageResponse,

    -- ** SendVoiceMessage
    SendVoiceMessage (SendVoiceMessage'),
    newSendVoiceMessage,
    SendVoiceMessageResponse (SendVoiceMessageResponse'),
    newSendVoiceMessageResponse,

    -- ** SetDefaultMessageType
    SetDefaultMessageType (SetDefaultMessageType'),
    newSetDefaultMessageType,
    SetDefaultMessageTypeResponse (SetDefaultMessageTypeResponse'),
    newSetDefaultMessageTypeResponse,

    -- ** SetDefaultSenderId
    SetDefaultSenderId (SetDefaultSenderId'),
    newSetDefaultSenderId,
    SetDefaultSenderIdResponse (SetDefaultSenderIdResponse'),
    newSetDefaultSenderIdResponse,

    -- ** SetTextMessageSpendLimitOverride
    SetTextMessageSpendLimitOverride (SetTextMessageSpendLimitOverride'),
    newSetTextMessageSpendLimitOverride,
    SetTextMessageSpendLimitOverrideResponse (SetTextMessageSpendLimitOverrideResponse'),
    newSetTextMessageSpendLimitOverrideResponse,

    -- ** SetVoiceMessageSpendLimitOverride
    SetVoiceMessageSpendLimitOverride (SetVoiceMessageSpendLimitOverride'),
    newSetVoiceMessageSpendLimitOverride,
    SetVoiceMessageSpendLimitOverrideResponse (SetVoiceMessageSpendLimitOverrideResponse'),
    newSetVoiceMessageSpendLimitOverrideResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateEventDestination
    UpdateEventDestination (UpdateEventDestination'),
    newUpdateEventDestination,
    UpdateEventDestinationResponse (UpdateEventDestinationResponse'),
    newUpdateEventDestinationResponse,

    -- ** UpdatePhoneNumber
    UpdatePhoneNumber (UpdatePhoneNumber'),
    newUpdatePhoneNumber,
    UpdatePhoneNumberResponse (UpdatePhoneNumberResponse'),
    newUpdatePhoneNumberResponse,

    -- ** UpdatePool
    UpdatePool (UpdatePool'),
    newUpdatePool,
    UpdatePoolResponse (UpdatePoolResponse'),
    newUpdatePoolResponse,

    -- * Types

    -- ** AccountAttributeName
    AccountAttributeName (..),

    -- ** AccountLimitName
    AccountLimitName (..),

    -- ** ConfigurationSetFilterName
    ConfigurationSetFilterName (..),

    -- ** DestinationCountryParameterKey
    DestinationCountryParameterKey (..),

    -- ** EventType
    EventType (..),

    -- ** KeywordAction
    KeywordAction (..),

    -- ** KeywordFilterName
    KeywordFilterName (..),

    -- ** MessageType
    MessageType (..),

    -- ** NumberCapability
    NumberCapability (..),

    -- ** NumberStatus
    NumberStatus (..),

    -- ** NumberType
    NumberType (..),

    -- ** OptedOutFilterName
    OptedOutFilterName (..),

    -- ** PhoneNumberFilterName
    PhoneNumberFilterName (..),

    -- ** PoolFilterName
    PoolFilterName (..),

    -- ** PoolOriginationIdentitiesFilterName
    PoolOriginationIdentitiesFilterName (..),

    -- ** PoolStatus
    PoolStatus (..),

    -- ** RequestableNumberType
    RequestableNumberType (..),

    -- ** SenderIdFilterName
    SenderIdFilterName (..),

    -- ** SpendLimitName
    SpendLimitName (..),

    -- ** VoiceId
    VoiceId (..),

    -- ** VoiceMessageBodyTextType
    VoiceMessageBodyTextType (..),

    -- ** AccountAttribute
    AccountAttribute (AccountAttribute'),
    newAccountAttribute,

    -- ** AccountLimit
    AccountLimit (AccountLimit'),
    newAccountLimit,

    -- ** CloudWatchLogsDestination
    CloudWatchLogsDestination (CloudWatchLogsDestination'),
    newCloudWatchLogsDestination,

    -- ** ConfigurationSetFilter
    ConfigurationSetFilter (ConfigurationSetFilter'),
    newConfigurationSetFilter,

    -- ** ConfigurationSetInformation
    ConfigurationSetInformation (ConfigurationSetInformation'),
    newConfigurationSetInformation,

    -- ** EventDestination
    EventDestination (EventDestination'),
    newEventDestination,

    -- ** KeywordFilter
    KeywordFilter (KeywordFilter'),
    newKeywordFilter,

    -- ** KeywordInformation
    KeywordInformation (KeywordInformation'),
    newKeywordInformation,

    -- ** KinesisFirehoseDestination
    KinesisFirehoseDestination (KinesisFirehoseDestination'),
    newKinesisFirehoseDestination,

    -- ** OptOutListInformation
    OptOutListInformation (OptOutListInformation'),
    newOptOutListInformation,

    -- ** OptedOutFilter
    OptedOutFilter (OptedOutFilter'),
    newOptedOutFilter,

    -- ** OptedOutNumberInformation
    OptedOutNumberInformation (OptedOutNumberInformation'),
    newOptedOutNumberInformation,

    -- ** OriginationIdentityMetadata
    OriginationIdentityMetadata (OriginationIdentityMetadata'),
    newOriginationIdentityMetadata,

    -- ** PhoneNumberFilter
    PhoneNumberFilter (PhoneNumberFilter'),
    newPhoneNumberFilter,

    -- ** PhoneNumberInformation
    PhoneNumberInformation (PhoneNumberInformation'),
    newPhoneNumberInformation,

    -- ** PoolFilter
    PoolFilter (PoolFilter'),
    newPoolFilter,

    -- ** PoolInformation
    PoolInformation (PoolInformation'),
    newPoolInformation,

    -- ** PoolOriginationIdentitiesFilter
    PoolOriginationIdentitiesFilter (PoolOriginationIdentitiesFilter'),
    newPoolOriginationIdentitiesFilter,

    -- ** SenderIdAndCountry
    SenderIdAndCountry (SenderIdAndCountry'),
    newSenderIdAndCountry,

    -- ** SenderIdFilter
    SenderIdFilter (SenderIdFilter'),
    newSenderIdFilter,

    -- ** SenderIdInformation
    SenderIdInformation (SenderIdInformation'),
    newSenderIdInformation,

    -- ** SnsDestination
    SnsDestination (SnsDestination'),
    newSnsDestination,

    -- ** SpendLimit
    SpendLimit (SpendLimit'),
    newSpendLimit,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Amazonka.PinpointSmsVoiceV2.AssociateOriginationIdentity
import Amazonka.PinpointSmsVoiceV2.CreateConfigurationSet
import Amazonka.PinpointSmsVoiceV2.CreateEventDestination
import Amazonka.PinpointSmsVoiceV2.CreateOptOutList
import Amazonka.PinpointSmsVoiceV2.CreatePool
import Amazonka.PinpointSmsVoiceV2.DeleteConfigurationSet
import Amazonka.PinpointSmsVoiceV2.DeleteDefaultMessageType
import Amazonka.PinpointSmsVoiceV2.DeleteDefaultSenderId
import Amazonka.PinpointSmsVoiceV2.DeleteEventDestination
import Amazonka.PinpointSmsVoiceV2.DeleteKeyword
import Amazonka.PinpointSmsVoiceV2.DeleteOptOutList
import Amazonka.PinpointSmsVoiceV2.DeleteOptedOutNumber
import Amazonka.PinpointSmsVoiceV2.DeletePool
import Amazonka.PinpointSmsVoiceV2.DeleteTextMessageSpendLimitOverride
import Amazonka.PinpointSmsVoiceV2.DeleteVoiceMessageSpendLimitOverride
import Amazonka.PinpointSmsVoiceV2.DescribeAccountAttributes
import Amazonka.PinpointSmsVoiceV2.DescribeAccountLimits
import Amazonka.PinpointSmsVoiceV2.DescribeConfigurationSets
import Amazonka.PinpointSmsVoiceV2.DescribeKeywords
import Amazonka.PinpointSmsVoiceV2.DescribeOptOutLists
import Amazonka.PinpointSmsVoiceV2.DescribeOptedOutNumbers
import Amazonka.PinpointSmsVoiceV2.DescribePhoneNumbers
import Amazonka.PinpointSmsVoiceV2.DescribePools
import Amazonka.PinpointSmsVoiceV2.DescribeSenderIds
import Amazonka.PinpointSmsVoiceV2.DescribeSpendLimits
import Amazonka.PinpointSmsVoiceV2.DisassociateOriginationIdentity
import Amazonka.PinpointSmsVoiceV2.Lens
import Amazonka.PinpointSmsVoiceV2.ListPoolOriginationIdentities
import Amazonka.PinpointSmsVoiceV2.ListTagsForResource
import Amazonka.PinpointSmsVoiceV2.PutKeyword
import Amazonka.PinpointSmsVoiceV2.PutOptedOutNumber
import Amazonka.PinpointSmsVoiceV2.ReleasePhoneNumber
import Amazonka.PinpointSmsVoiceV2.RequestPhoneNumber
import Amazonka.PinpointSmsVoiceV2.SendTextMessage
import Amazonka.PinpointSmsVoiceV2.SendVoiceMessage
import Amazonka.PinpointSmsVoiceV2.SetDefaultMessageType
import Amazonka.PinpointSmsVoiceV2.SetDefaultSenderId
import Amazonka.PinpointSmsVoiceV2.SetTextMessageSpendLimitOverride
import Amazonka.PinpointSmsVoiceV2.SetVoiceMessageSpendLimitOverride
import Amazonka.PinpointSmsVoiceV2.TagResource
import Amazonka.PinpointSmsVoiceV2.Types
import Amazonka.PinpointSmsVoiceV2.UntagResource
import Amazonka.PinpointSmsVoiceV2.UpdateEventDestination
import Amazonka.PinpointSmsVoiceV2.UpdatePhoneNumber
import Amazonka.PinpointSmsVoiceV2.UpdatePool
import Amazonka.PinpointSmsVoiceV2.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'PinpointSmsVoiceV2'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
