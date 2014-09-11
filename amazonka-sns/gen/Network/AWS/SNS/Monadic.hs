{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SNS.Monadic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This module is provided for convenience. It offers an alternative to the
-- common idiom of supplying required fields to an operations's smart constructor,
-- using the operation's lenses to modify additional fields, and then sending
-- or paginating the request.
--
-- As an example: using "Network.AWS.SNS" with the smart constructor and
-- basic lens syntax, before explicitly calling 'send':
--
-- @
-- import Control.Monad.Trans.AWS
-- import Network.AWS.SNS
--
-- send $ (mkOperationName w x)
--      & onLensField1 .~ y
--      & onLensField2 .~ z
-- @
--
-- Versus using "Network.AWS.SNS.Monadic" with the 'State' operator variants from
-- "Control.Lens.Setter" such as '.=' to modify any additional request
-- parameters before sending:
--
-- @
-- import Control.Applicative
-- import Network.AWS.SNS.Monadic
--
-- operationName w x $ do
--     onLensField1 .= y
--     onLensField2 .= z
--
-- -- Or to void any additional parameters outside of those required using 'Control.Applicative.empty':
-- operationName w x empty
-- @
--
module Network.AWS.SNS.Monadic
    (
    -- * AddPermission
    -- $AddPermission
      addPermission
    , addPermissionCatch

    -- * ConfirmSubscription
    -- $ConfirmSubscription
    , confirmSubscription
    , confirmSubscriptionCatch

    -- * CreatePlatformApplication
    -- $CreatePlatformApplication
    , createPlatformApplication
    , createPlatformApplicationCatch

    -- * CreatePlatformEndpoint
    -- $CreatePlatformEndpoint
    , createPlatformEndpoint
    , createPlatformEndpointCatch

    -- * CreateTopic
    -- $CreateTopic
    , createTopic
    , createTopicCatch

    -- * DeleteEndpoint
    -- $DeleteEndpoint
    , deleteEndpoint
    , deleteEndpointCatch

    -- * DeletePlatformApplication
    -- $DeletePlatformApplication
    , deletePlatformApplication
    , deletePlatformApplicationCatch

    -- * DeleteTopic
    -- $DeleteTopic
    , deleteTopic
    , deleteTopicCatch

    -- * GetEndpointAttributes
    -- $GetEndpointAttributes
    , getEndpointAttributes
    , getEndpointAttributesCatch

    -- * GetPlatformApplicationAttributes
    -- $GetPlatformApplicationAttributes
    , getPlatformApplicationAttributes
    , getPlatformApplicationAttributesCatch

    -- * GetSubscriptionAttributes
    -- $GetSubscriptionAttributes
    , getSubscriptionAttributes
    , getSubscriptionAttributesCatch

    -- * GetTopicAttributes
    -- $GetTopicAttributes
    , getTopicAttributes
    , getTopicAttributesCatch

    -- * ListEndpointsByPlatformApplication
    -- $ListEndpointsByPlatformApplication
    , listEndpointsByPlatformApplication
    , listEndpointsByPlatformApplicationCatch

    -- * ListPlatformApplications
    -- $ListPlatformApplications
    , listPlatformApplications
    , listPlatformApplicationsCatch

    -- * ListSubscriptions
    -- $ListSubscriptions
    , listSubscriptions
    , listSubscriptionsCatch

    -- * ListSubscriptionsByTopic
    -- $ListSubscriptionsByTopic
    , listSubscriptionsByTopic
    , listSubscriptionsByTopicCatch

    -- * ListTopics
    -- $ListTopics
    , listTopics
    , listTopicsCatch

    -- * Publish
    -- $Publish
    , publish
    , publishCatch

    -- * RemovePermission
    -- $RemovePermission
    , removePermission
    , removePermissionCatch

    -- * SetEndpointAttributes
    -- $SetEndpointAttributes
    , setEndpointAttributes
    , setEndpointAttributesCatch

    -- * SetPlatformApplicationAttributes
    -- $SetPlatformApplicationAttributes
    , setPlatformApplicationAttributes
    , setPlatformApplicationAttributesCatch

    -- * SetSubscriptionAttributes
    -- $SetSubscriptionAttributes
    , setSubscriptionAttributes
    , setSubscriptionAttributesCatch

    -- * SetTopicAttributes
    -- $SetTopicAttributes
    , setTopicAttributes
    , setTopicAttributesCatch

    -- * Subscribe
    -- $Subscribe
    , subscribe
    , subscribeCatch

    -- * Unsubscribe
    -- $Unsubscribe
    , unsubscribe
    , unsubscribeCatch

    -- * Re-exported
    , module Network.AWS.SNS

    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.SNS

type ServiceEr = Er SNS

-- $AddPermission
-- Adds a statement to a topic's access control policy, granting access for
-- the specified AWS accounts to the specified actions.
-- http://sns.us-east-1.amazonaws.com/
-- ?TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Test
-- &amp;ActionName.member.1=Publish
-- &amp;ActionName.member.2=GetTopicAttributes &amp;Label=NewPermission
-- &amp;AWSAccountId.member.1=987654321000
-- &amp;AWSAccountId.member.2=876543210000 &amp;Action=AddPermission
-- &amp;SignatureVersion=2 &amp;SignatureMethod=HmacSHA256
-- &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z &amp;AWSAccessKeyId=(AWS Access
-- Key ID) &amp;Signature=k%2FAU%2FKp13pjndwJ7rr1sZszy6MZMlOhRBCHx1ZaZFiw%3D
-- &lt;AddPermissionResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;6a213e4e-33a8-11df-9540-99d0768312d3&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/AddPermissionResponse&gt;.
--
-- See: 'Network.AWS.SNS.AddPermission'

addPermission :: ( MonadCatch m
                 , MonadResource m
                 , MonadError AWS.Error m
                 , MonadReader Env m
                 )
    => Text -- ^ 'apTopicArn'
    -> Text -- ^ 'apLabel'
    -> [Text] -- ^ 'apAWSAccountId'
    -> [Text] -- ^ 'apActionName'
    -> m AddPermissionResponse
addPermission p1 p2 p3 p4 =
    send (mkAddPermission p1 p2 p3 p4)

addPermissionCatch :: ( MonadCatch m
                      , MonadResource m
                      , MonadReader Env m
                      )
    => Text -- ^ 'apTopicArn'
    -> Text -- ^ 'apLabel'
    -> [Text] -- ^ 'apAWSAccountId'
    -> [Text] -- ^ 'apActionName'
    -> m (Either ServiceEr AddPermissionResponse)
addPermissionCatch p1 p2 p3 p4 =
    sendCatch (mkAddPermission p1 p2 p3 p4)

-- $ConfirmSubscription
-- Verifies an endpoint owner's intent to receive messages by validating the
-- token sent to the endpoint by an earlier Subscribe action. If the token is
-- valid, the action creates a new subscription and returns its Amazon
-- Resource Name (ARN). This call requires an AWS signature only when the
-- AuthenticateOnUnsubscribe flag is set to "true".
-- https://sns.us-east-1.amazonaws.com/ ?Action=ConfirmSubscription
-- &amp;TopicArn=arn:aws:sns:us-east-1:123456789012:My-Topic
-- &amp;Token=51b2ff3edb475b7d91550e0ab6edf0c1de2a34e6ebaf6
-- c2262a001bcb7e051c43aa00022ceecce70bd2a67b2042da8d8
-- eb47fef7a4e4e942d23e7fa56146b9ee35da040b4b8af564cc4
-- 184a7391c834cb75d75c22981f776ad1ce8805e9bab29da2329
-- 985337bb8095627907b46c8577c8440556b6f86582a95475802
-- 6f41fc62041c4b3f67b0f5921232b5dae5aaca1 &lt;ConfirmSubscriptionResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ConfirmSubscriptionResult&gt;
-- &lt;SubscriptionArn&gt;arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca&lt;/SubscriptionArn&gt;
-- &lt;/ConfirmSubscriptionResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;7a50221f-3774-11df-a9b7-05d48da6f042&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/ConfirmSubscriptionResponse&gt;.
--
-- See: 'Network.AWS.SNS.ConfirmSubscription'

confirmSubscription :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => Text -- ^ 'csTopicArn'
    -> Text -- ^ 'csToken'
    -> State ConfirmSubscription a
    -> m ConfirmSubscriptionResponse
confirmSubscription p1 p2 s =
    send $ (mkConfirmSubscription p1 p2) &~ s

confirmSubscriptionCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => Text -- ^ 'csTopicArn'
    -> Text -- ^ 'csToken'
    -> State ConfirmSubscription a
    -> m (Either ServiceEr ConfirmSubscriptionResponse)
confirmSubscriptionCatch p1 p2 s =
    sendCatch $ (mkConfirmSubscription p1 p2) &~ s

-- $CreatePlatformApplication
-- Creates a platform application object for one of the supported push
-- notification services, such as APNS and GCM, to which devices and mobile
-- apps may register. You must specify PlatformPrincipal and
-- PlatformCredential attributes when using the CreatePlatformApplication
-- action. The PlatformPrincipal is received from the notification service.
-- For APNS/APNS_SANDBOX, PlatformPrincipal is "SSL certificate". For GCM,
-- PlatformPrincipal is not applicable. For ADM, PlatformPrincipal is "client
-- id". The PlatformCredential is also received from the notification service.
-- For APNS/APNS_SANDBOX, PlatformCredential is "private key". For GCM,
-- PlatformCredential is "API key". For ADM, PlatformCredential is "client
-- secret". The PlatformApplicationArn that is returned when using
-- CreatePlatformApplication is then used as an attribute for the
-- CreatePlatformEndpoint action. For more information, see Using Amazon SNS
-- Mobile Push Notifications. POST http://sns.us-west-2.amazonaws.com/
-- HTTP/1.1 ... Attributes.entry.2.key=PlatformPrincipal
-- &amp;SignatureMethod=HmacSHA256
-- &amp;Attributes.entry.1.value=AIzaSyClE2lcV2zEKTLYYo645zfk2jhQPFeyxDo
-- &amp;Attributes.entry.2.value=There+is+no+principal+for+GCM
-- &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &amp;Signature=82sHzg1Wfbgisw3i%2BHA2OgBmRktsqUKFinknkq3u%2FQ4%3D
-- &amp;Timestamp=2013-07-01T15%3A49%3A50.354Z &amp;Name=gcmpushapp
-- &amp;Attributes.entry.1.key=PlatformCredential
-- &amp;Action=CreatePlatformApplication &amp;Version=2010-03-31
-- &amp;SignatureVersion=2 &amp;Platform=GCM HTTP/1.1 200 OK ...
-- &lt;CreatePlatformApplicationResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;CreatePlatformApplicationResult&gt;
-- &lt;PlatformApplicationArn&gt;arn:aws:sns:us-west-2:123456789012:app/GCM/gcmpushapp&lt;/PlatformApplicationArn&gt;
-- &lt;/CreatePlatformApplicationResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;b6f0e78b-e9d4-5a0e-b973-adc04e8a4ff9&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/CreatePlatformApplicationResponse&gt;.
--
-- See: 'Network.AWS.SNS.CreatePlatformApplication'

createPlatformApplication :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => Text -- ^ 'cpaName'
    -> Text -- ^ 'cpaPlatform'
    -> Map Text Text -- ^ 'cpaAttributes'
    -> m CreatePlatformApplicationResponse
createPlatformApplication p1 p2 p3 =
    send (mkCreatePlatformApplication p1 p2 p3)

createPlatformApplicationCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'cpaName'
    -> Text -- ^ 'cpaPlatform'
    -> Map Text Text -- ^ 'cpaAttributes'
    -> m (Either ServiceEr CreatePlatformApplicationResponse)
createPlatformApplicationCatch p1 p2 p3 =
    sendCatch (mkCreatePlatformApplication p1 p2 p3)

-- $CreatePlatformEndpoint
-- Creates an endpoint for a device and mobile app on one of the supported
-- push notification services, such as GCM and APNS. CreatePlatformEndpoint
-- requires the PlatformApplicationArn that is returned from
-- CreatePlatformApplication. The EndpointArn that is returned when using
-- CreatePlatformEndpoint can then be used by the Publish action to send a
-- message to a mobile app or by the Subscribe action for subscription to a
-- topic. The CreatePlatformEndpoint action is idempotent, so if the requester
-- already owns an endpoint with the same device token and attributes, that
-- endpoint's ARN is returned without creating a new endpoint. For more
-- information, see Using Amazon SNS Mobile Push Notifications. When using
-- CreatePlatformEndpoint with Baidu, two attributes must be provided:
-- ChannelId and UserId. The token field must also contain the ChannelId. For
-- more information, see Creating an Amazon SNS Endpoint for Baidu. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- PlatformApplicationArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aapp%2FGCM%2Fgcmpushapp
-- &amp;Action=CreatePlatformEndpoint &amp;SignatureMethod=HmacSHA256
-- &amp;CustomUserData=UserId%3D27576823
-- &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &amp;Token=APA91bGi7fFachkC1xjlqT66VYEucGHochmf1VQAr9k...jsM0PKPxKhddCzx6paEsyay9Zn3D4wNUJb8m6HZrBEXAMPLE
-- &amp;SignatureVersion=2 &amp;Version=2010-03-31
-- &amp;Signature=Rg5vXBS6OfgPtWkt1u32p1w14uiGh%2BKOicvXNWTEz2w%3D
-- &amp;Timestamp=2013-07-01T15%3A49%3A50.598Z HTTP/1.1 200 OK ...
-- &lt;CreatePlatformEndpointResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;CreatePlatformEndpointResult&gt;
-- &lt;EndpointArn&gt;arn:aws:sns:us-west-2:123456789012:endpoint/GCM/gcmpushapp/5e3e9847-3183-3f18-a7e8-671c3a57d4b3&lt;/EndpointArn&gt;
-- &lt;/CreatePlatformEndpointResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;6613341d-3e15-53f7-bf3c-7e56994ba278&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/CreatePlatformEndpointResponse&gt;.
--
-- See: 'Network.AWS.SNS.CreatePlatformEndpoint'

createPlatformEndpoint :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'cpePlatformApplicationArn'
    -> Text -- ^ 'cpeToken'
    -> State CreatePlatformEndpoint a
    -> m CreatePlatformEndpointResponse
createPlatformEndpoint p1 p2 s =
    send $ (mkCreatePlatformEndpoint p1 p2) &~ s

createPlatformEndpointCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'cpePlatformApplicationArn'
    -> Text -- ^ 'cpeToken'
    -> State CreatePlatformEndpoint a
    -> m (Either ServiceEr CreatePlatformEndpointResponse)
createPlatformEndpointCatch p1 p2 s =
    sendCatch $ (mkCreatePlatformEndpoint p1 p2) &~ s

-- $CreateTopic
-- Creates a topic to which notifications can be published. Users can create
-- at most 3000 topics. For more information, see http://aws.amazon.com/sns.
-- This action is idempotent, so if the requester already owns a topic with
-- the specified name, that topic's ARN is returned without creating a new
-- topic. http://sns.us-east-1.amazonaws.com/ ?Name=My-Topic
-- &amp;Action=CreateTopic &amp;SignatureVersion=2
-- &amp;SignatureMethod=HmacSHA256 &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &amp;AWSAccessKeyId=(AWS Access Key ID)
-- &amp;Signature=gfzIF53exFVdpSNb8AiwN3Lv%2FNYXh6S%2Br3yySK70oX4%3D
-- &lt;CreateTopicResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;CreateTopicResult&gt;
-- &lt;TopicArn&gt;arn:aws:sns:us-east-1:123456789012:My-Topic&lt;/TopicArn&gt;
-- &lt;/CreateTopicResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;a8dec8b3-33a4-11df-8963-01868b7c937a&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/CreateTopicResponse&gt;.
--
-- See: 'Network.AWS.SNS.CreateTopic'

createTopic :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => Text -- ^ 'ctName'
    -> m CreateTopicResponse
createTopic p1 =
    send (mkCreateTopic p1)

createTopicCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => Text -- ^ 'ctName'
    -> m (Either ServiceEr CreateTopicResponse)
createTopicCatch p1 =
    sendCatch (mkCreateTopic p1)

-- $DeleteEndpoint
-- Deletes the endpoint from Amazon SNS. This action is idempotent. For more
-- information, see Using Amazon SNS Mobile Push Notifications. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ... Action=DeleteEndpoint
-- &amp;SignatureMethod=HmacSHA256 &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &amp;EndpointArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aendpoint%2FGCM%2Fgcmpushapp%2F5e3e9847-3183-3f18-a7e8-671c3a57d4b3
-- &amp;SignatureVersion=2 &amp;Version=2010-03-31
-- &amp;Signature=LIc6GI3JbNhmHBEDmSxzZp648XPe5CMeFny%2BTQFtomQ%3D
-- &amp;Timestamp=2013-07-01T23%3A00%3A12.456Z HTTP/1.1 200 OK ...
-- &lt;DeleteEndpointResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;c1d2b191-353c-5a5f-8969-fbdd3900afa8&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/DeleteEndpointResponse&gt;.
--
-- See: 'Network.AWS.SNS.DeleteEndpoint'

deleteEndpoint :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'deEndpointArn'
    -> m DeleteEndpointResponse
deleteEndpoint p1 =
    send (mkDeleteEndpoint p1)

deleteEndpointCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'deEndpointArn'
    -> m (Either ServiceEr DeleteEndpointResponse)
deleteEndpointCatch p1 =
    sendCatch (mkDeleteEndpoint p1)

-- $DeletePlatformApplication
-- Deletes a platform application object for one of the supported push
-- notification services, such as APNS and GCM. For more information, see
-- Using Amazon SNS Mobile Push Notifications. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- PlatformApplicationArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aapp%2FGCM%2Fgcmpushapp
-- &amp;Action=DeletePlatformApplication &amp;SignatureMethod=HmacSHA256
-- &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &amp;SignatureVersion=2
-- &amp;Version=2010-03-31
-- &amp;Signature=Mh7X%2BQo%2BGpcm5B1IpkovBaRiJCJOqvFlIOYzL62SGrg%3D
-- &amp;Timestamp=2013-07-01T23%3A02%3A03.872Z HTTP/1.1 200 OK ...
-- &lt;DeletePlatformApplicationResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;097dac18-7a77-5823-a8dd-e65476dcb037&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/DeletePlatformApplicationResponse&gt;.
--
-- See: 'Network.AWS.SNS.DeletePlatformApplication'

deletePlatformApplication :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => Text -- ^ 'dpaPlatformApplicationArn'
    -> m DeletePlatformApplicationResponse
deletePlatformApplication p1 =
    send (mkDeletePlatformApplication p1)

deletePlatformApplicationCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'dpaPlatformApplicationArn'
    -> m (Either ServiceEr DeletePlatformApplicationResponse)
deletePlatformApplicationCatch p1 =
    sendCatch (mkDeletePlatformApplication p1)

-- $DeleteTopic
-- Deletes a topic and all its subscriptions. Deleting a topic might prevent
-- some messages previously sent to the topic from being delivered to
-- subscribers. This action is idempotent, so deleting a topic that does not
-- exist does not result in an error. http://sns.us-east-1.amazonaws.com/
-- &amp;TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic
-- &amp;Action=DeleteTopic &amp;SignatureVersion=2
-- &amp;SignatureMethod=HmacSHA256 &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &amp;AWSAccessKeyId=(AWS Access Key Id)
-- &amp;Signature=mQA3nJI%2BcmAIY7r8HCArGElSqPX5JG4UGzF4yo0RygE%3D
-- &lt;DeleteTopicResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;f3aa9ac9-3c3d-11df-8235-9dab105e9c32&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/DeleteTopicResponse&gt;.
--
-- See: 'Network.AWS.SNS.DeleteTopic'

deleteTopic :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => Text -- ^ 'dtTopicArn'
    -> m DeleteTopicResponse
deleteTopic p1 =
    send (mkDeleteTopic p1)

deleteTopicCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => Text -- ^ 'dtTopicArn'
    -> m (Either ServiceEr DeleteTopicResponse)
deleteTopicCatch p1 =
    sendCatch (mkDeleteTopic p1)

-- $GetEndpointAttributes
-- Retrieves the endpoint attributes for a device on one of the supported push
-- notification services, such as GCM and APNS. For more information, see
-- Using Amazon SNS Mobile Push Notifications. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- Action=GetEndpointAttributes &amp;SignatureMethod=HmacSHA256
-- &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &amp;EndpointArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aendpoint%2FGCM%2Fgcmpushapp%2F5e3e9847-3183-3f18-a7e8-671c3a57d4b3
-- &amp;SignatureVersion=2 &amp;Version=2010-03-31
-- &amp;Signature=%2B2egbEoT4npw3p5H3wiIdzZBoTn4KI3UWmMFyBsHH9c%3D
-- &amp;Timestamp=2013-07-01T22%3A44%3A56.515Z HTTP/1.1 200 OK ...
-- &lt;GetEndpointAttributesResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;GetEndpointAttributesResult&gt; &lt;Attributes&gt; &lt;entry&gt;
-- &lt;key&gt;Enabled&lt;/key&gt; &lt;value&gt;true&lt;/value&gt;
-- &lt;/entry&gt; &lt;entry&gt; &lt;key&gt;CustomUserData&lt;/key&gt;
-- &lt;value&gt;UserId=01234567&lt;/value&gt; &lt;/entry&gt; &lt;entry&gt;
-- &lt;key&gt;Token&lt;/key&gt;
-- &lt;value&gt;APA91bGi7fFachkC1xjlqT66VYEucGHochmf1VQAr9k...jsM0PKPxKhddCzx6paEsyay9Zn3D4wNUJb8m6HZrBEXAMPLE&lt;/value&gt;
-- &lt;/entry&gt; &lt;/Attributes&gt; &lt;/GetEndpointAttributesResult&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;6c725a19-a142-5b77-94f9-1055a9ea04e7&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/GetEndpointAttributesResponse&gt;.
--
-- See: 'Network.AWS.SNS.GetEndpointAttributes'

getEndpointAttributes :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'geaEndpointArn'
    -> m GetEndpointAttributesResponse
getEndpointAttributes p1 =
    send (mkGetEndpointAttributes p1)

getEndpointAttributesCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'geaEndpointArn'
    -> m (Either ServiceEr GetEndpointAttributesResponse)
getEndpointAttributesCatch p1 =
    sendCatch (mkGetEndpointAttributes p1)

-- $GetPlatformApplicationAttributes
-- Retrieves the attributes of the platform application object for the
-- supported push notification services, such as APNS and GCM. For more
-- information, see Using Amazon SNS Mobile Push Notifications. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- PlatformApplicationArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aapp%2FGCM%2Fgcmpushapp
-- &amp;Action=GetPlatformApplicationAttributes
-- &amp;SignatureMethod=HmacSHA256 &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &amp;SignatureVersion=2 &amp;Version=2010-03-31
-- &amp;Signature=UGMaCq8CCJGSYXO9Ehr2VuHIBYSe6WbxkqgMKRslTK4%3D
-- &amp;Timestamp=2013-07-01T22%3A40%3A50.643Z HTTP/1.1 200 OK ...
-- &lt;GetPlatformApplicationAttributesResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;GetPlatformApplicationAttributesResult&gt; &lt;Attributes&gt;
-- &lt;entry&gt; &lt;key&gt;AllowEndpointPolicies&lt;/key&gt;
-- &lt;value&gt;false&lt;/value&gt; &lt;/entry&gt; &lt;/Attributes&gt;
-- &lt;/GetPlatformApplicationAttributesResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;74848df2-87f6-55ed-890c-c7be80442462&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt;
-- &lt;/GetPlatformApplicationAttributesResponse&gt;.
--
-- See: 'Network.AWS.SNS.GetPlatformApplicationAttributes'

getPlatformApplicationAttributes :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadError AWS.Error m
                                    , MonadReader Env m
                                    )
    => Text -- ^ 'gpaaPlatformApplicationArn'
    -> m GetPlatformApplicationAttributesResponse
getPlatformApplicationAttributes p1 =
    send (mkGetPlatformApplicationAttributes p1)

getPlatformApplicationAttributesCatch :: ( MonadCatch m
                                         , MonadResource m
                                         , MonadReader Env m
                                         )
    => Text -- ^ 'gpaaPlatformApplicationArn'
    -> m (Either ServiceEr GetPlatformApplicationAttributesResponse)
getPlatformApplicationAttributesCatch p1 =
    sendCatch (mkGetPlatformApplicationAttributes p1)

-- $GetSubscriptionAttributes
-- Returns all of the properties of a subscription.
-- http://sns.us-east-1.amazonaws.com/
-- ?SubscriptionArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic%3A80289ba6-0fd4-4079-afb4-ce8c8260f0ca
-- &amp;Action=GetSubscriptionAttributes &amp;SignatureVersion=2
-- &amp;SignatureMethod=HmacSHA256 &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &amp;AWSAccessKeyId=(AWS Access Key Id)
-- &amp;Signature=92lBGRVq0%2BxhaACaBGqtdemy%2Bi9isfgyTljCbJM80Yk%3D
-- &lt;GetSubscriptionAttributesResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;GetSubscriptionAttributesResult&gt; &lt;Attributes&gt; &lt;entry&gt;
-- &lt;key&gt;Owner&lt;/key&gt; &lt;value&gt;123456789012&lt;/value&gt;
-- &lt;/entry&gt; &lt;entry&gt; &lt;key&gt;DeliveryPolicy&lt;/key&gt;
-- &lt;value&gt;{&amp;quot;healthyRetryPolicy&amp;quot;:{&amp;quot;numRetries&amp;quot;:10}}&lt;/value&gt;
-- &lt;/entry&gt; &lt;entry&gt; &lt;key&gt;SubscriptionArn&lt;/key&gt;
-- &lt;value&gt;arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca&lt;/value&gt;
-- &lt;/entry&gt; &lt;/Attributes&gt; &lt;/GetSubscriptionAttributesResult&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;057f074c-33a7-11df-9540-99d0768312d3&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/GetTopicAttributesResponse&gt;.
--
-- See: 'Network.AWS.SNS.GetSubscriptionAttributes'

getSubscriptionAttributes :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => Text -- ^ 'gsaSubscriptionArn'
    -> m GetSubscriptionAttributesResponse
getSubscriptionAttributes p1 =
    send (mkGetSubscriptionAttributes p1)

getSubscriptionAttributesCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'gsaSubscriptionArn'
    -> m (Either ServiceEr GetSubscriptionAttributesResponse)
getSubscriptionAttributesCatch p1 =
    sendCatch (mkGetSubscriptionAttributes p1)

-- $GetTopicAttributes
-- Returns all of the properties of a topic. Topic properties returned might
-- differ based on the authorization of the user.
-- http://sns.us-east-1.amazonaws.com/
-- ?TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic
-- &amp;Action=GetTopicAttributes &amp;SignatureVersion=2
-- &amp;SignatureMethod=HmacSHA256 &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &amp;AWSAccessKeyId=(AWS Access Key Id)
-- &amp;Signature=92lBGRVq0%2BxhaACaBGqtdemy%2Bi9isfgyTljCbJM80Yk%3D
-- &lt;GetTopicAttributesResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;GetTopicAttributesResult&gt; &lt;Attributes&gt; &lt;entry&gt;
-- &lt;key&gt;Owner&lt;/key&gt; &lt;value&gt;123456789012&lt;/value&gt;
-- &lt;/entry&gt; &lt;entry&gt; &lt;key&gt;Policy&lt;/key&gt; &lt;value&gt;{
-- &amp;quot;Version&amp;quot;:&amp;quot;2008-10-17&amp;quot;,&amp;quot;Id&amp;quot;:&amp;quot;us-east-1/698519295917/test__default_policy_ID&amp;quot;,&amp;quot;Statement&amp;quot;
-- :
-- [{&amp;quot;Effect&amp;quot;:&amp;quot;Allow&amp;quot;,&amp;quot;Sid&amp;quot;:&amp;quot;us-east-1/698519295917/test__default_statement_ID&amp;quot;,&amp;quot;Principal&amp;quot;
-- : {&amp;quot;AWS&amp;quot;:
-- &amp;quot;*&amp;quot;},&amp;quot;Action&amp;quot;:[&amp;quot;SNS:GetTopicAttributes&amp;quot;,&amp;quot;SNS:SetTopicAttributes&amp;quot;,&amp;quot;SNS:AddPermission&amp;quot;,&amp;quot;SNS:RemovePermission&amp;quot;,&amp;quot;SNS:DeleteTopic&amp;quot;,&amp;quot;SNS:Subscribe&amp;quot;,&amp;quot;SNS:ListSubscriptionsByTopic&amp;quot;,&amp;quot;SNS:Publish&amp;quot;,&amp;quot;SNS:Receive&amp;quot;],&amp;quot;Resource&amp;quot;:&amp;quot;arn:aws:sns:us-east-1:698519295917:test&amp;quot;,&amp;quot;Condition&amp;quot;
-- : {&amp;quot;StringLike&amp;quot; : {&amp;quot;AWS:SourceArn&amp;quot;:
-- &amp;quot;arn:aws:*:*:698519295917:*&amp;quot;}}}]}&lt;/value&gt;
-- &lt;/entry&gt; &lt;entry&gt; &lt;key&gt;TopicArn&lt;/key&gt;
-- &lt;value&gt;arn:aws:sns:us-east-1:123456789012:My-Topic&lt;/value&gt;
-- &lt;/entry&gt; &lt;/Attributes&gt; &lt;/GetTopicAttributesResult&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;057f074c-33a7-11df-9540-99d0768312d3&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/GetTopicAttributesResponse&gt;.
--
-- See: 'Network.AWS.SNS.GetTopicAttributes'

getTopicAttributes :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'gtaTopicArn'
    -> m GetTopicAttributesResponse
getTopicAttributes p1 =
    send (mkGetTopicAttributes p1)

getTopicAttributesCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'gtaTopicArn'
    -> m (Either ServiceEr GetTopicAttributesResponse)
getTopicAttributesCatch p1 =
    sendCatch (mkGetTopicAttributes p1)

-- $ListEndpointsByPlatformApplication
-- Lists the endpoints and endpoint attributes for devices in a supported push
-- notification service, such as GCM and APNS. The results for
-- ListEndpointsByPlatformApplication are paginated and return a limited list
-- of endpoints, up to 100. If additional records are available after the
-- first page results, then a NextToken string will be returned. To receive
-- the next page, you call ListEndpointsByPlatformApplication again using the
-- NextToken string received from the previous call. When there are no more
-- records to return, NextToken will be null. For more information, see Using
-- Amazon SNS Mobile Push Notifications. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- PlatformApplicationArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aapp%2FGCM%2Fgcmpushapp
-- &amp;Action=ListEndpointsByPlatformApplication
-- &amp;SignatureMethod=HmacSHA256 &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &amp;SignatureVersion=2 &amp;Version=2010-03-31
-- &amp;Signature=e6H4sJSCRBBlh%2BaigB%2FtYgp4%2Bjl7dikAQ6WKf%2BMTwNM%3D
-- &amp;Timestamp=2013-07-01T23%3A00%3A52.515Z HTTP/1.1 200 OK ...
-- &lt;ListEndpointsByPlatformApplicationResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ListEndpointsByPlatformApplicationResult&gt; &lt;Endpoints&gt;
-- &lt;member&gt;
-- &lt;EndpointArn&gt;arn:aws:sns:us-west-2:123456789012:endpoint/GCM/gcmpushapp/5e3e9847-3183-3f18-a7e8-671c3a57d4b3&lt;/EndpointArn&gt;
-- &lt;Attributes&gt; &lt;entry&gt; &lt;key&gt;Enabled&lt;/key&gt;
-- &lt;value&gt;true&lt;/value&gt; &lt;/entry&gt; &lt;entry&gt;
-- &lt;key&gt;CustomUserData&lt;/key&gt;
-- &lt;value&gt;UserId=27576823&lt;/value&gt; &lt;/entry&gt; &lt;entry&gt;
-- &lt;key&gt;Token&lt;/key&gt;
-- &lt;value&gt;APA91bGi7fFachkC1xjlqT66VYEucGHochmf1VQAr9k...jsM0PKPxKhddCzx6paEsyay9Zn3D4wNUJb8m6HZrBEXAMPLE&lt;/value&gt;
-- &lt;/entry&gt; &lt;/Attributes&gt; &lt;/member&gt; &lt;/Endpoints&gt;
-- &lt;/ListEndpointsByPlatformApplicationResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;9a48768c-dac8-5a60-aec0-3cc27ea08d96&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt;
-- &lt;/ListEndpointsByPlatformApplicationResponse&gt;.
--
-- See: 'Network.AWS.SNS.ListEndpointsByPlatformApplication'

listEndpointsByPlatformApplication :: ( MonadCatch m
                                      , MonadResource m
                                      , MonadError AWS.Error m
                                      , MonadReader Env m
                                      )
    => Text -- ^ 'lebpaPlatformApplicationArn'
    -> State ListEndpointsByPlatformApplication a
    -> Source m ListEndpointsByPlatformApplicationResponse
listEndpointsByPlatformApplication p1 s =
    paginate $ (mkListEndpointsByPlatformApplication p1) &~ s

listEndpointsByPlatformApplicationCatch :: ( MonadCatch m
                                           , MonadResource m
                                           , MonadReader Env m
                                           )
    => Text -- ^ 'lebpaPlatformApplicationArn'
    -> State ListEndpointsByPlatformApplication a
    -> Source m (Either ServiceEr ListEndpointsByPlatformApplicationResponse)
listEndpointsByPlatformApplicationCatch p1 s =
    paginateCatch $ (mkListEndpointsByPlatformApplication p1) &~ s

-- $ListPlatformApplications
-- Lists the platform application objects for the supported push notification
-- services, such as APNS and GCM. The results for ListPlatformApplications
-- are paginated and return a limited list of applications, up to 100. If
-- additional records are available after the first page results, then a
-- NextToken string will be returned. To receive the next page, you call
-- ListPlatformApplications using the NextToken string received from the
-- previous call. When there are no more records to return, NextToken will be
-- null. For more information, see Using Amazon SNS Mobile Push Notifications.
-- POST http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- Action=ListPlatformApplications &amp;SignatureMethod=HmacSHA256
-- &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &amp;SignatureVersion=2
-- &amp;Version=2010-03-31
-- &amp;Signature=drVbTuyR5N9e88WJMNPzBOjNFNvawkCaMfZI0xa9kIQ%3D
-- &amp;Timestamp=2013-07-01T22%3A33%3A55.618Z HTTP/1.1 200 OK ...
-- &lt;ListPlatformApplicationsResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ListPlatformApplicationsResult&gt; &lt;PlatformApplications&gt;
-- &lt;member&gt;
-- &lt;PlatformApplicationArn&gt;arn:aws:sns:us-west-2:123456789012:app/APNS_SANDBOX/apnspushapp&lt;/PlatformApplicationArn&gt;
-- &lt;Attributes&gt; &lt;entry&gt;
-- &lt;key&gt;AllowEndpointPolicies&lt;/key&gt;
-- &lt;value&gt;false&lt;/value&gt; &lt;/entry&gt; &lt;/Attributes&gt;
-- &lt;/member&gt; &lt;member&gt;
-- &lt;PlatformApplicationArn&gt;arn:aws:sns:us-west-2:123456789012:app/GCM/gcmpushapp&lt;/PlatformApplicationArn&gt;
-- &lt;Attributes&gt; &lt;entry&gt;
-- &lt;key&gt;AllowEndpointPolicies&lt;/key&gt;
-- &lt;value&gt;false&lt;/value&gt; &lt;/entry&gt; &lt;/Attributes&gt;
-- &lt;/member&gt; &lt;/PlatformApplications&gt;
-- &lt;/ListPlatformApplicationsResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;315a335e-85d8-52df-9349-791283cbb529&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/ListPlatformApplicationsResponse&gt;.
--
-- See: 'Network.AWS.SNS.ListPlatformApplications'

listPlatformApplications :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => State ListPlatformApplications a
    -> Source m ListPlatformApplicationsResponse
listPlatformApplications s =
    paginate (mkListPlatformApplications &~ s)

listPlatformApplicationsCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => State ListPlatformApplications a
    -> Source m (Either ServiceEr ListPlatformApplicationsResponse)
listPlatformApplicationsCatch s =
    paginateCatch (mkListPlatformApplications &~ s)

-- $ListSubscriptions
-- Returns a list of the requester's subscriptions. Each call returns a
-- limited list of subscriptions, up to 100. If there are more subscriptions,
-- a NextToken is also returned. Use the NextToken parameter in a new
-- ListSubscriptions call to get further results.
-- http://sns.us-east-1.amazonaws.com/ &amp;Action=ListSubscriptions
-- &amp;SignatureVersion=2 &amp;SignatureMethod=HmacSHA256
-- &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z &amp;AWSAccessKeyId=(AWS Access
-- Key ID) &amp;Signature=SZmBxEPqfs9R7xxhSt6C1b7PnOEvg%2BSVyyMYJfLRFCA%3D
-- &lt;ListSubscriptionsResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ListSubscriptionsResult&gt; &lt;Subscriptions&gt; &lt;member&gt;
-- &lt;TopicArn&gt;arn:aws:sns:us-east-1:698519295917:My-Topic&lt;/TopicArn&gt;
-- &lt;Protocol&gt;email&lt;/Protocol&gt;
-- &lt;SubscriptionArn&gt;arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca&lt;/SubscriptionArn&gt;
-- &lt;Owner&gt;123456789012&lt;/Owner&gt;
-- &lt;Endpoint&gt;example@amazon.com&lt;/Endpoint&gt; &lt;/member&gt;
-- &lt;/Subscriptions&gt; &lt;/ListSubscriptionsResult&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;384ac68d-3775-11df-8963-01868b7c937a&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/ListSubscriptionsResponse&gt;.
--
-- See: 'Network.AWS.SNS.ListSubscriptions'

listSubscriptions :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => State ListSubscriptions a
    -> Source m ListSubscriptionsResponse
listSubscriptions s =
    paginate (mkListSubscriptions &~ s)

listSubscriptionsCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => State ListSubscriptions a
    -> Source m (Either ServiceEr ListSubscriptionsResponse)
listSubscriptionsCatch s =
    paginateCatch (mkListSubscriptions &~ s)

-- $ListSubscriptionsByTopic
-- Returns a list of the subscriptions to a specific topic. Each call returns
-- a limited list of subscriptions, up to 100. If there are more
-- subscriptions, a NextToken is also returned. Use the NextToken parameter in
-- a new ListSubscriptionsByTopic call to get further results.
-- http://sns.us-east-1.amazonaws.com/
-- ?TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic
-- &amp;Action=ListSubscriptionsByTopic &amp;SignatureVersion=2
-- &amp;SignatureMethod=HmacSHA256 &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &amp;AWSAccessKeyId=(AWS Access Key ID)
-- &amp;Signature=SZmBxEPqfs9R7xxhSt6C1b7PnOEvg%2BSVyyMYJfLRFCA%3D
-- &lt;ListSubscriptionsByTopicResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ListSubscriptionsByTopicResult&gt; &lt;Subscriptions&gt; &lt;member&gt;
-- &lt;TopicArn&gt;arn:aws:sns:us-east-1:123456789012:My-Topic&lt;/TopicArn&gt;
-- &lt;Protocol&gt;email&lt;/Protocol&gt;
-- &lt;SubscriptionArn&gt;arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca&lt;/SubscriptionArn&gt;
-- &lt;Owner&gt;123456789012&lt;/Owner&gt;
-- &lt;Endpoint&gt;example@amazon.com&lt;/Endpoint&gt; &lt;/member&gt;
-- &lt;/Subscriptions&gt; &lt;/ListSubscriptionsByTopicResult&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;b9275252-3774-11df-9540-99d0768312d3&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/ListSubscriptionsByTopicResponse&gt;.
--
-- See: 'Network.AWS.SNS.ListSubscriptionsByTopic'

listSubscriptionsByTopic :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env m
                            )
    => Text -- ^ 'lsbtTopicArn'
    -> State ListSubscriptionsByTopic a
    -> Source m ListSubscriptionsByTopicResponse
listSubscriptionsByTopic p1 s =
    paginate $ (mkListSubscriptionsByTopic p1) &~ s

listSubscriptionsByTopicCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env m
                                 )
    => Text -- ^ 'lsbtTopicArn'
    -> State ListSubscriptionsByTopic a
    -> Source m (Either ServiceEr ListSubscriptionsByTopicResponse)
listSubscriptionsByTopicCatch p1 s =
    paginateCatch $ (mkListSubscriptionsByTopic p1) &~ s

-- $ListTopics
-- Returns a list of the requester's topics. Each call returns a limited list
-- of topics, up to 100. If there are more topics, a NextToken is also
-- returned. Use the NextToken parameter in a new ListTopics call to get
-- further results. http://sns.us-east-1.amazonaws.com/ ?Action=ListTopics
-- &amp;SignatureVersion=2 &amp;SignatureMethod=HmacSHA256
-- &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z &amp;AWSAccessKeyId=(AWS Access
-- Key ID) &amp;Signature=tPg1qKNTNVPydnL3Yx5Fqm2O9GxCr9vh3EF5r9%2F5%2BJs%3D
-- &lt;ListTopicsResponse xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ListTopicsResult&gt; &lt;Topics&gt; &lt;member&gt;
-- &lt;TopicArn&gt;arn:aws:sns:us-east-1:123456789012:My-Topic&lt;/TopicArn&gt;
-- &lt;/member&gt; &lt;/Topics&gt; &lt;/ListTopicsResult&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;3f1478c7-33a9-11df-9540-99d0768312d3&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/ListTopicsResponse&gt;.
--
-- See: 'Network.AWS.SNS.ListTopics'

listTopics :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              )
    => State ListTopics a
    -> Source m ListTopicsResponse
listTopics s =
    paginate (mkListTopics &~ s)

listTopicsCatch :: ( MonadCatch m
                   , MonadResource m
                   , MonadReader Env m
                   )
    => State ListTopics a
    -> Source m (Either ServiceEr ListTopicsResponse)
listTopicsCatch s =
    paginateCatch (mkListTopics &~ s)

-- $Publish
-- Sends a message to all of a topic's subscribed endpoints. When a messageId
-- is returned, the message has been saved and Amazon SNS will attempt to
-- deliver it to the topic's subscribers shortly. The format of the outgoing
-- message to each subscribed endpoint depends on the notification protocol
-- selected. To use the Publish action for sending a message to a mobile
-- endpoint, such as an app on a Kindle device or mobile phone, you must
-- specify the EndpointArn. The EndpointArn is returned when making a call
-- with the CreatePlatformEndpoint action. The second example below shows a
-- request and response for publishing to a mobile endpoint. The following
-- example publishes the same message to all protocols:
-- http://sns.us-east-1.amazonaws.com/ ?Subject=My%20first%20message
-- &amp;TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A698519295917%3AMy-Topic
-- &amp;Message=Hello%20world%21 &amp;Action=Publish &amp;SignatureVersion=2
-- &amp;SignatureMethod=HmacSHA256 &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &amp;Signature=9GZysQ4Jpnz%2BHklqM7VFTvEcjR2LIUtn6jW47054xxE%3D Use the
-- following JSON object format for the Message parameter to send different
-- messages to each protocol (linebreaks added for readability): { "default" :
-- "some message", "email" : "some email message", "email-json" : "some
-- email-json message", "http" : "some http message", "https" : "some https
-- message", "sqs" : "some sqs message" } &lt;PublishResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt; &lt;PublishResult&gt;
-- &lt;MessageId&gt;94f20ce6-13c5-43a0-9a9e-ca52d816e90b&lt;/MessageId&gt;
-- &lt;/PublishResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;f187a3c1-376f-11df-8963-01868b7c937a&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/PublishResponse&gt; POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ... Action=Publish
-- &amp;Message=%7B%22default%22%3A%22This+is+the+default+Message%22%2C%22APNS_SANDBOX%22%3A%22%7B+%5C%22aps%5C%22+%3A+%7B+%5C%22alert%5C%22+%3A+%5C%22You+have+got+email.%5C%22%2C+%5C%22badge%5C%22+%3A+9%2C%5C%22sound%5C%22+%3A%5C%22default%5C%22%7D%7D%22%7D
-- 
-- &amp;TargetArn=arn%3Aaws%3Asns%3Aus-west-2%3A803981987763%3Aendpoint%2FAPNS_SANDBOX%2Fpushapp%2F98e9ced9-f136-3893-9d60-776547eafebb
-- &amp;SignatureMethod=HmacSHA256 &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &amp;SignatureVersion=2 &amp;Version=2010-03-31
-- &amp;Signature=vmqc4XRupKAxsDAdN4j4Ayw5LQljXMps3kss4bkDfCk%3D
-- &amp;Timestamp=2013-07-18T22%3A44%3A09.452Z &amp;MessageStructure=json
-- HTTP/1.1 200 OK ... &lt;PublishResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt; &lt;PublishResult&gt;
-- &lt;MessageId&gt;567910cd-659e-55d4-8ccb-5aaf14679dc0&lt;/MessageId&gt;
-- &lt;/PublishResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;d74b8436-ae13-5ab4-a9ff-ce54dfea72a0&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/PublishResponse&gt;.
--
-- See: 'Network.AWS.SNS.Publish'

publish :: ( MonadCatch m
           , MonadResource m
           , MonadError AWS.Error m
           , MonadReader Env m
           )
    => Text -- ^ 'pMessage'
    -> State Publish a
    -> m PublishResponse
publish p3 s =
    send $ (mkPublish p3) &~ s

publishCatch :: ( MonadCatch m
                , MonadResource m
                , MonadReader Env m
                )
    => Text -- ^ 'pMessage'
    -> State Publish a
    -> m (Either ServiceEr PublishResponse)
publishCatch p3 s =
    sendCatch $ (mkPublish p3) &~ s

-- $RemovePermission
-- Removes a statement from a topic's access control policy.
-- http://sns.us-east-1.amazonaws.com/
-- ?TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Test
-- &amp;Label=NewPermission &amp;Action=RemovePermission
-- &amp;SignatureVersion=2 &amp;SignatureMethod=HmacSHA256
-- &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z &amp;AWSAccessKeyId=(AWS Access
-- Key ID) &amp;Signature=N1abwRY9i7zaSQmbAlm71pPf9EEFOqNbQL1alzw2yCg%3D
-- &lt;RemovePermissionResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;d170b150-33a8-11df-995a-2d6fbe836cc1&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/RemovePermissionResponse&gt;.
--
-- See: 'Network.AWS.SNS.RemovePermission'

removePermission :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'rpTopicArn'
    -> Text -- ^ 'rpLabel'
    -> m RemovePermissionResponse
removePermission p1 p2 =
    send (mkRemovePermission p1 p2)

removePermissionCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'rpTopicArn'
    -> Text -- ^ 'rpLabel'
    -> m (Either ServiceEr RemovePermissionResponse)
removePermissionCatch p1 p2 =
    sendCatch (mkRemovePermission p1 p2)

-- $SetEndpointAttributes
-- Sets the attributes for an endpoint for a device on one of the supported
-- push notification services, such as GCM and APNS. For more information, see
-- Using Amazon SNS Mobile Push Notifications. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- Attributes.entry.1.key=CustomUserData &amp;Action=SetEndpointAttributes
-- &amp;SignatureMethod=HmacSHA256
-- &amp;Attributes.entry.1.value=My+custom+userdata
-- &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &amp;EndpointArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aendpoint%2FGCM%2Fgcmpushapp%2F5e3e9847-3183-3f18-a7e8-671c3a57d4b3
-- &amp;SignatureVersion=2 &amp;Version=2010-03-31
-- &amp;Signature=CFTGfGOS5vgSU3%2FZgv2h%2FJdWgr2JQdDJSrUU9k38wSM%3D
-- &amp;Timestamp=2013-07-01T22%3A56%3A45.582Z HTTP/1.1 200 OK ...
-- &lt;SetEndpointAttributesResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;2fe0bfc7-3e85-5ee5-a9e2-f58b35e85f6a&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/SetEndpointAttributesResponse&gt;.
--
-- See: 'Network.AWS.SNS.SetEndpointAttributes'

setEndpointAttributes :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'seaEndpointArn'
    -> Map Text Text -- ^ 'seaAttributes'
    -> m SetEndpointAttributesResponse
setEndpointAttributes p1 p2 =
    send (mkSetEndpointAttributes p1 p2)

setEndpointAttributesCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'seaEndpointArn'
    -> Map Text Text -- ^ 'seaAttributes'
    -> m (Either ServiceEr SetEndpointAttributesResponse)
setEndpointAttributesCatch p1 p2 =
    sendCatch (mkSetEndpointAttributes p1 p2)

-- $SetPlatformApplicationAttributes
-- Sets the attributes of the platform application object for the supported
-- push notification services, such as APNS and GCM. For more information, see
-- Using Amazon SNS Mobile Push Notifications. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- Attributes.entry.1.key=EventEndpointCreated&amp;PlatformApplicationArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aapp%2FGCM%2Fgcmpushapp
-- &amp;Action=SetPlatformApplicationAttributes
-- &amp;SignatureMethod=HmacSHA256
-- &amp;Attributes.entry.1.value=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Atopicarn
-- &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &amp;SignatureVersion=2
-- &amp;Version=2010-03-31
-- &amp;Signature=06L2TsW3jiH%2FGKDYuT8w4NojSrTf4Ig2GKqGeJPhPT4%3D
-- &amp;Timestamp=2013-07-01T22%3A53%3A17.800Z HTTP/1.1 200 OK ...
-- &lt;SetPlatformApplicationAttributesResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;cf577bcc-b3dc-5463-88f1-3180b9412395&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt;
-- &lt;/SetPlatformApplicationAttributesResponse&gt;.
--
-- See: 'Network.AWS.SNS.SetPlatformApplicationAttributes'

setPlatformApplicationAttributes :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadError AWS.Error m
                                    , MonadReader Env m
                                    )
    => Text -- ^ 'spaaPlatformApplicationArn'
    -> Map Text Text -- ^ 'spaaAttributes'
    -> m SetPlatformApplicationAttributesResponse
setPlatformApplicationAttributes p1 p2 =
    send (mkSetPlatformApplicationAttributes p1 p2)

setPlatformApplicationAttributesCatch :: ( MonadCatch m
                                         , MonadResource m
                                         , MonadReader Env m
                                         )
    => Text -- ^ 'spaaPlatformApplicationArn'
    -> Map Text Text -- ^ 'spaaAttributes'
    -> m (Either ServiceEr SetPlatformApplicationAttributesResponse)
setPlatformApplicationAttributesCatch p1 p2 =
    sendCatch (mkSetPlatformApplicationAttributes p1 p2)

-- $SetSubscriptionAttributes
-- Allows a subscription owner to set an attribute of the topic to a new
-- value. The following example sets the delivery policy to 5 total retries
-- http://sns.us-east-1.amazonaws.com/
-- ?AttributeValue={"healthyRetryPolicy":{"numRetries":5}}
-- &amp;SubscriptionArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic%3A80289ba6-0fd4-4079-afb4-ce8c8260f0ca
-- &amp;AttributeName=DeliveryPolicy &amp;Action=SetSubscriptionAttributes
-- &amp;SignatureVersion=2 &amp;SignatureMethod=HmacSHA256
-- &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z &amp;AWSAccessKeyId=(AWS Access
-- Key Id) &amp;Signature=mQA3nJI%2BcmAIY7r8HCArGElSqPX5JG4UGzF4yo0RygE%3D The
-- JSON format for the DeliveryPolicy AttributeValue (linebreaks added for
-- readability): { "healthyRetryPolicy": { "minDelayTarget": &lt;int&gt;,
-- "maxDelayTarget": &lt;int&gt;, "numRetries": &lt;int&gt;,
-- "numMaxDelayRetries": &lt;int&gt;, "backoffFunction":
-- "&lt;linear|arithmetic|geometric|exponential&gt;" }, "throttlePolicy": {
-- "maxReceivesPerSecond": &lt;int&gt; } }
-- &lt;SetSubscriptionAttributesResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;a8763b99-33a7-11df-a9b7-05d48da6f042&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/SetSubscriptionAttributesResponse&gt;.
--
-- See: 'Network.AWS.SNS.SetSubscriptionAttributes'

setSubscriptionAttributes :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => Text -- ^ 'ssaSubscriptionArn'
    -> Text -- ^ 'ssaAttributeName'
    -> State SetSubscriptionAttributes a
    -> m SetSubscriptionAttributesResponse
setSubscriptionAttributes p1 p2 s =
    send $ (mkSetSubscriptionAttributes p1 p2) &~ s

setSubscriptionAttributesCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'ssaSubscriptionArn'
    -> Text -- ^ 'ssaAttributeName'
    -> State SetSubscriptionAttributes a
    -> m (Either ServiceEr SetSubscriptionAttributesResponse)
setSubscriptionAttributesCatch p1 p2 s =
    sendCatch $ (mkSetSubscriptionAttributes p1 p2) &~ s

-- $SetTopicAttributes
-- Allows a topic owner to set an attribute of the topic to a new value. The
-- following example sets the DisplayName attribute to MyTopicName
-- http://sns.us-east-1.amazonaws.com/ ?AttributeValue=MyTopicName
-- &amp;TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic
-- &amp;AttributeName=DisplayName &amp;Action=SetTopicAttributes
-- &amp;SignatureVersion=2 &amp;SignatureMethod=HmacSHA256
-- &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z &amp;AWSAccessKeyId=(AWS Access
-- Key Id) &amp;Signature=mQA3nJI%2BcmAIY7r8HCArGElSqPX5JG4UGzF4yo0RygE%3D The
-- following example sets the delivery policy to 5 total retries
-- http://sns.us-east-1.amazonaws.com/
-- ?AttributeValue={"http":{"defaultHealthyRetryPolicy":{"numRetries":5}}}
-- &amp;TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic
-- &amp;AttributeName=DeliveryPolicy &amp;Action=SetTopicAttributes
-- &amp;SignatureVersion=2 &amp;SignatureMethod=HmacSHA256
-- &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z &amp;AWSAccessKeyId=(AWS Access
-- Key Id) &amp;Signature=mQA3nJI%2BcmAIY7r8HCArGElSqPX5JG4UGzF4yo0RygE%3D The
-- JSON format for the DeliveryPolicy AttributeValue (linebreaks added for
-- readability): { "http": { "defaultHealthyRetryPolicy": { "minDelayTarget":
-- &lt;int&gt;, "maxDelayTarget": &lt;int&gt;, "numRetries": &lt;int&gt;,
-- "numMaxDelayRetries": &lt;int&gt;, "backoffFunction":
-- "&lt;linear|arithmetic|geometric|exponential&gt;" },
-- "disableSubscriptionOverrides": &lt;boolean&gt;, "defaultThrottlePolicy": {
-- "maxReceivesPerSecond": &lt;int&gt; } } &lt;SetTopicAttributesResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;a8763b99-33a7-11df-a9b7-05d48da6f042&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/SetTopicAttributesResponse&gt;.
--
-- See: 'Network.AWS.SNS.SetTopicAttributes'

setTopicAttributes :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'staTopicArn'
    -> Text -- ^ 'staAttributeName'
    -> State SetTopicAttributes a
    -> m SetTopicAttributesResponse
setTopicAttributes p1 p2 s =
    send $ (mkSetTopicAttributes p1 p2) &~ s

setTopicAttributesCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'staTopicArn'
    -> Text -- ^ 'staAttributeName'
    -> State SetTopicAttributes a
    -> m (Either ServiceEr SetTopicAttributesResponse)
setTopicAttributesCatch p1 p2 s =
    sendCatch $ (mkSetTopicAttributes p1 p2) &~ s

-- $Subscribe
-- Prepares to subscribe an endpoint by sending the endpoint a confirmation
-- message. To actually create a subscription, the endpoint owner must call
-- the ConfirmSubscription action with the token from the confirmation
-- message. Confirmation tokens are valid for three days. The following
-- example Query request subscribes an SQS queue to an SNS topic. For more
-- information, see Subscribe Queue to Amazon SNS Topic in the Amazon SQS
-- Developer Guide. http://sns.us-west-2.amazonaws.com/ &amp;Action=Subscribe
-- &amp;Endpoint=arn%3Aaws%3Asqs%3Aus-west-2%3A123456789012%3AMyQueue
-- &amp;Version=2010-03-31 &amp;Protocol=sqs
-- &amp;TopicArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3AMyTopic
-- &lt;SubscribeResponse xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;SubscribeResult&gt;
-- &lt;SubscriptionArn&gt;arn:aws:sns:us-west-2:123456789012:MyTopic:6b0e71bd-7e97-4d97-80ce-4a0994e55286&lt;/SubscriptionArn&gt;
-- &lt;/SubscribeResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;c4407779-24a4-56fa-982c-3d927f93a775&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/SubscribeResponse&gt;.
--
-- See: 'Network.AWS.SNS.Subscribe'

subscribe :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
    => Text -- ^ 's1TopicArn'
    -> Text -- ^ 's1Protocol'
    -> State Subscribe a
    -> m SubscribeResponse
subscribe p1 p2 s =
    send $ (mkSubscribe p1 p2) &~ s

subscribeCatch :: ( MonadCatch m
                  , MonadResource m
                  , MonadReader Env m
                  )
    => Text -- ^ 's1TopicArn'
    -> Text -- ^ 's1Protocol'
    -> State Subscribe a
    -> m (Either ServiceEr SubscribeResponse)
subscribeCatch p1 p2 s =
    sendCatch $ (mkSubscribe p1 p2) &~ s

-- $Unsubscribe
-- Deletes a subscription. If the subscription requires authentication for
-- deletion, only the owner of the subscription or the topic's owner can
-- unsubscribe, and an AWS signature is required. If the Unsubscribe call does
-- not require authentication and the requester is not the subscription owner,
-- a final cancellation message is delivered to the endpoint, so that the
-- endpoint owner can easily resubscribe to the topic if the Unsubscribe
-- request was unintended. http://sns.us-east-1.amazonaws.com/
-- ?SubscriptionArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic%3A80289ba6-0fd4-4079-afb4-ce8c8260f0ca
-- &amp;Action=Unsubscribe &amp;SignatureVersion=2
-- &amp;SignatureMethod=HmacSHA256 &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &amp;AWSAccessKeyId=(AWS Access Key ID)
-- &amp;Signature=e8IwhPzuWeMvPDVrN7jUVxasd3Wv2LuO8x6rE23VCv8%3D
-- &lt;UnsubscribeResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;18e0ac39-3776-11df-84c0-b93cc1666b84&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/UnsubscribeResponse&gt;.
--
-- See: 'Network.AWS.SNS.Unsubscribe'

unsubscribe :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => Text -- ^ 'uSubscriptionArn'
    -> m UnsubscribeResponse
unsubscribe p1 =
    send (mkUnsubscribe p1)

unsubscribeCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => Text -- ^ 'uSubscriptionArn'
    -> m (Either ServiceEr UnsubscribeResponse)
unsubscribeCatch p1 =
    sendCatch (mkUnsubscribe p1)
