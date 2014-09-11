{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudFormation.Monadic
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
-- As an example: using "Network.AWS.CloudFormation" with the smart constructor and
-- basic lens syntax, before explicitly calling 'send':
--
-- @
-- import Control.Monad.Trans.AWS
-- import Network.AWS.CloudFormation
--
-- send $ (mkOperationName w x)
--      & onLensField1 .~ y
--      & onLensField2 .~ z
-- @
--
-- Versus using "Network.AWS.CloudFormation.Monadic" with the 'State' operator variants from
-- "Control.Lens.Setter" such as '.=' to modify any additional request
-- parameters before sending:
--
-- @
-- import Control.Applicative
-- import Network.AWS.CloudFormation.Monadic
--
-- operationName w x $ do
--     onLensField1 .= y
--     onLensField2 .= z
--
-- -- Or to void any additional parameters outside of those required using 'Control.Applicative.empty':
-- operationName w x empty
-- @
--
module Network.AWS.CloudFormation.Monadic
    (
    -- * CancelUpdateStack
    -- $CancelUpdateStack
      cancelUpdateStack
    , cancelUpdateStackCatch

    -- * CreateStack
    -- $CreateStack
    , createStack
    , createStackCatch

    -- * DeleteStack
    -- $DeleteStack
    , deleteStack
    , deleteStackCatch

    -- * DescribeStackEvents
    -- $DescribeStackEvents
    , describeStackEvents
    , describeStackEventsCatch

    -- * DescribeStackResource
    -- $DescribeStackResource
    , describeStackResource
    , describeStackResourceCatch

    -- * DescribeStackResources
    -- $DescribeStackResources
    , describeStackResources
    , describeStackResourcesCatch

    -- * DescribeStacks
    -- $DescribeStacks
    , describeStacks
    , describeStacksCatch

    -- * EstimateTemplateCost
    -- $EstimateTemplateCost
    , estimateTemplateCost
    , estimateTemplateCostCatch

    -- * GetStackPolicy
    -- $GetStackPolicy
    , getStackPolicy
    , getStackPolicyCatch

    -- * GetTemplate
    -- $GetTemplate
    , getTemplate
    , getTemplateCatch

    -- * ListStackResources
    -- $ListStackResources
    , listStackResources
    , listStackResourcesCatch

    -- * ListStacks
    -- $ListStacks
    , listStacks
    , listStacksCatch

    -- * SetStackPolicy
    -- $SetStackPolicy
    , setStackPolicy
    , setStackPolicyCatch

    -- * UpdateStack
    -- $UpdateStack
    , updateStack
    , updateStackCatch

    -- * ValidateTemplate
    -- $ValidateTemplate
    , validateTemplate
    , validateTemplateCatch

    -- * Re-exported
    , module Network.AWS.CloudFormation

    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.CloudFormation

type ServiceEr = Er CloudFormation

-- $CancelUpdateStack
-- Cancels an update on the specified stack. If the call completes
-- successfully, the stack will roll back the update and revert to the
-- previous stack configuration. Only stacks that are in the
-- UPDATE_IN_PROGRESS state can be canceled.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=CancelUpdateStack
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature].
--
-- See: 'Network.AWS.CloudFormation.CancelUpdateStack'

cancelUpdateStack :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => Text -- ^ 'cusStackName'
    -> m CancelUpdateStackResponse
cancelUpdateStack p1 =
    send (mkCancelUpdateStack p1)

cancelUpdateStackCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => Text -- ^ 'cusStackName'
    -> m (Either ServiceEr CancelUpdateStackResponse)
cancelUpdateStackCatch p1 =
    sendCatch (mkCancelUpdateStack p1)

-- $CreateStack
-- Creates a stack as specified in the template. After the call completes
-- successfully, the stack creation starts. You can check the status of the
-- stack via the DescribeStacks API.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=CreateStack
-- &StackName=MyStack &TemplateBody=[Template Document]
-- &NotificationARNs.member.1=arn:aws:sns:us-east-1:1234567890:my-topic
-- &Parameters.member.1.ParameterKey=AvailabilityZone
-- &Parameters.member.1.ParameterValue=us-east-1a &Version=2010-05-15
-- &SignatureVersion=2 &Timestamp=2010-07-27T22%3A26%3A28.000Z
-- &AWSAccessKeyId=[AWS Access KeyID] &Signature=[Signature]
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83.
-- 
--
-- See: 'Network.AWS.CloudFormation.CreateStack'

createStack :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => Text -- ^ 'csStackName'
    -> State CreateStack a
    -> m CreateStackResponse
createStack p1 s =
    send $ (mkCreateStack p1) &~ s

createStackCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => Text -- ^ 'csStackName'
    -> State CreateStack a
    -> m (Either ServiceEr CreateStackResponse)
createStackCatch p1 s =
    sendCatch $ (mkCreateStack p1) &~ s

-- $DeleteStack
-- Deletes a specified stack. Once the call completes successfully, stack
-- deletion starts. Deleted stacks do not show up in the DescribeStacks API if
-- the deletion has been completed successfully.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=DeleteStack
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature].
--
-- See: 'Network.AWS.CloudFormation.DeleteStack'

deleteStack :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => Text -- ^ 'dsStackName'
    -> m DeleteStackResponse
deleteStack p1 =
    send (mkDeleteStack p1)

deleteStackCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => Text -- ^ 'dsStackName'
    -> m (Either ServiceEr DeleteStackResponse)
deleteStackCatch p1 =
    sendCatch (mkDeleteStack p1)

-- $DescribeStackEvents
-- Returns all stack related events for a specified stack. For more
-- information about a stack's event history, go to Stacks in the AWS
-- CloudFormation User Guide. You can list events for stacks that have failed
-- to create or have been deleted by specifying the unique stack identifier
-- (stack ID). https://cloudformation.us-east-1.amazonaws.com/
-- ?Action=DescribeStackEvents &StackName=MyStack &Version=2010-05-15
-- &SignatureVersion=2 &Timestamp=2010-07-27T22%3A26%3A28.000Z
-- &AWSAccessKeyId=[AWS Access KeyID] &Signature=[Signature] Event-1-Id
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83
-- MyStack MyStack MyStack_One AWS::CloudFormation::Stack 2010-07-27T22:26:28Z
-- CREATE_IN_PROGRESS User initiated Event-2-Id
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83
-- MyStack MyDBInstance MyStack_DB1 AWS::SecurityGroup 2010-07-27T22:27:28Z
-- CREATE_IN_PROGRESS {"GroupDescription":...} Event-3-Id
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83
-- MyStack MySG1 MyStack_SG1 AWS:: SecurityGroup 2010-07-27T22:28:28Z
-- CREATE_COMPLETE.
--
-- See: 'Network.AWS.CloudFormation.DescribeStackEvents'

describeStackEvents :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => Text -- ^ 'dseStackName'
    -> State DescribeStackEvents a
    -> Source m DescribeStackEventsResponse
describeStackEvents p1 s =
    paginate $ (mkDescribeStackEvents p1) &~ s

describeStackEventsCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => Text -- ^ 'dseStackName'
    -> State DescribeStackEvents a
    -> Source m (Either ServiceEr DescribeStackEventsResponse)
describeStackEventsCatch p1 s =
    paginateCatch $ (mkDescribeStackEvents p1) &~ s

-- $DescribeStackResource
-- Returns a description of the specified resource in the specified stack. For
-- deleted stacks, DescribeStackResource returns resource information for up
-- to 90 days after the stack has been deleted.
-- https://cloudformation.us-east-1.amazonaws.com/
-- ?Action=DescribeStackResource &StackName=MyStack
-- &LogicalResourceId=MyDBInstance &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2011-07-08T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature]
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83
-- MyStack MyDBInstance MyStack_DB1 AWS::RDS::DBInstance 2011-07-07T22:27:28Z
-- CREATE_COMPLETE.
--
-- See: 'Network.AWS.CloudFormation.DescribeStackResource'

describeStackResource :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'dsrStackName'
    -> Text -- ^ 'dsrLogicalResourceId'
    -> m DescribeStackResourceResponse
describeStackResource p1 p2 =
    send (mkDescribeStackResource p1 p2)

describeStackResourceCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'dsrStackName'
    -> Text -- ^ 'dsrLogicalResourceId'
    -> m (Either ServiceEr DescribeStackResourceResponse)
describeStackResourceCatch p1 p2 =
    sendCatch (mkDescribeStackResource p1 p2)

-- $DescribeStackResources
-- Returns AWS resource descriptions for running and deleted stacks. If
-- StackName is specified, all the associated resources that are part of the
-- stack are returned. If PhysicalResourceId is specified, the associated
-- resources of the stack that the resource belongs to are returned. Only the
-- first 100 resources will be returned. If your stack has more resources than
-- this, you should use ListStackResources instead. For deleted stacks,
-- DescribeStackResources returns resource information for up to 90 days after
-- the stack has been deleted. You must specify either StackName or
-- PhysicalResourceId, but not both. In addition, you can specify
-- LogicalResourceId to filter the returned result. For more information about
-- resources, the LogicalResourceId and PhysicalResourceId, go to the AWS
-- CloudFormation User Guide. A ValidationError is returned if you specify
-- both StackName and PhysicalResourceId in the same request.
-- https://cloudformation.us-east-1.amazonaws.com/
-- ?Action=DescribeStackResources &StackName=MyStack &Version=2010-05-15
-- &SignatureVersion=2 &Timestamp=2010-07-27T22%3A26%3A28.000Z
-- &AWSAccessKeyId=[AWS Access KeyID] &Signature=[Signature]
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83
-- MyStack MyDBInstance MyStack_DB1 AWS::DBInstance 2010-07-27T22:27:28Z
-- CREATE_COMPLETE
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83
-- MyStack MyAutoScalingGroup MyStack_ASG1 AWS::AutoScalingGroup
-- 2010-07-27T22:28:28Z CREATE_IN_PROGRESS.
--
-- See: 'Network.AWS.CloudFormation.DescribeStackResources'

describeStackResources :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => State DescribeStackResources a
    -> m DescribeStackResourcesResponse
describeStackResources s =
    send (mkDescribeStackResources &~ s)

describeStackResourcesCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => State DescribeStackResources a
    -> m (Either ServiceEr DescribeStackResourcesResponse)
describeStackResourcesCatch s =
    sendCatch (mkDescribeStackResources &~ s)

-- $DescribeStacks
-- Returns the description for the specified stack; if no stack name was
-- specified, then it returns the description for all the stacks created.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=DescribeStacks
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature] MyStack
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83
-- 2010-07-27T22:28:28Z CREATE_COMPLETE false StartPage
-- http://my-load-balancer.amazonaws.com:80/index.html.
--
-- See: 'Network.AWS.CloudFormation.DescribeStacks'

describeStacks :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => State DescribeStacks a
    -> Source m DescribeStacksResponse
describeStacks s =
    paginate (mkDescribeStacks &~ s)

describeStacksCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => State DescribeStacks a
    -> Source m (Either ServiceEr DescribeStacksResponse)
describeStacksCatch s =
    paginateCatch (mkDescribeStacks &~ s)

-- $EstimateTemplateCost
-- Returns the estimated monthly cost of a template. The return value is an
-- AWS Simple Monthly Calculator URL with a query string that describes the
-- resources required to run the template.
-- https://cloudformation.us-east-1.amazonaws.com/
-- ?Action=EstimateTemplateCost &TemplateURL=
-- https://s3.amazonaws.com/cloudformation-samples-us-east-1/Drupal_Simple.template
-- &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2011-12-04T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature]
-- http://calculator.s3.amazonaws.com/calc5.html?key=cf-2e351785-e821-450c-9d58-625e1e1ebfb6.
-- 
--
-- See: 'Network.AWS.CloudFormation.EstimateTemplateCost'

estimateTemplateCost :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => State EstimateTemplateCost a
    -> m EstimateTemplateCostResponse
estimateTemplateCost s =
    send (mkEstimateTemplateCost &~ s)

estimateTemplateCostCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => State EstimateTemplateCost a
    -> m (Either ServiceEr EstimateTemplateCostResponse)
estimateTemplateCostCatch s =
    sendCatch (mkEstimateTemplateCost &~ s)

-- $GetStackPolicy
-- Returns the stack policy for a specified stack. If a stack doesn't have a
-- policy, a null value is returned.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=GetStackPolicy
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature] "{ "Statement" : [ { "Effect" : "Deny", "Action" :
-- "Update:*", "Principal" : "*", "Resource" :
-- "LogicalResourceId/ProductionDatabase" }, { "Effect" : "Allow", "Action" :
-- "Update:*", "Principal" : "*", "Resource" : "*" } ] }.
--
-- See: 'Network.AWS.CloudFormation.GetStackPolicy'

getStackPolicy :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'gspStackName'
    -> m GetStackPolicyResponse
getStackPolicy p1 =
    send (mkGetStackPolicy p1)

getStackPolicyCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'gspStackName'
    -> m (Either ServiceEr GetStackPolicyResponse)
getStackPolicyCatch p1 =
    sendCatch (mkGetStackPolicy p1)

-- $GetTemplate
-- Returns the template body for a specified stack. You can get the template
-- for running or deleted stacks. For deleted stacks, GetTemplate returns the
-- template for up to 90 days after the stack has been deleted. If the
-- template does not exist, a ValidationError is returned.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=GetTemplate
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature] "{ "AWSTemplateFormatVersion" : "2010-09-09",
-- "Description" : "Simple example", "Resources" : { "MySQS" : { "Type" :
-- "AWS::SQS::Queue", "Properties" : { } } } }.
--
-- See: 'Network.AWS.CloudFormation.GetTemplate'

getTemplate :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => Text -- ^ 'gtStackName'
    -> m GetTemplateResponse
getTemplate p1 =
    send (mkGetTemplate p1)

getTemplateCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => Text -- ^ 'gtStackName'
    -> m (Either ServiceEr GetTemplateResponse)
getTemplateCatch p1 =
    sendCatch (mkGetTemplate p1)

-- $ListStackResources
-- Returns descriptions of all resources of the specified stack. For deleted
-- stacks, ListStackResources returns resource information for up to 90 days
-- after the stack has been deleted.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=ListStackResources
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2011-07-08T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature] CREATE_COMPLETE DBSecurityGroup 2011-06-21T20:15:58Z
-- gmarcteststack-dbsecuritygroup-1s5m0ez5lkk6w AWS::RDS::DBSecurityGroup
-- CREATE_COMPLETE SampleDB 2011-06-21T20:25:57Z MyStack-sampledb-ycwhk1v830lx
-- AWS::RDS::DBInstance CREATE_COMPLETE SampleApplication 2011-06-21T20:26:12Z
-- MyStack-SampleApplication-1MKNASYR3RBQL AWS::ElasticBeanstalk::Application
-- CREATE_COMPLETE SampleEnvironment 2011-06-21T20:28:48Z
-- myst-Samp-1AGU6ERZX6M3Q AWS::ElasticBeanstalk::Environment CREATE_COMPLETE
-- AlarmTopic 2011-06-21T20:29:06Z
-- arn:aws:sns:us-east-1:803981987763:MyStack-AlarmTopic-SW4IQELG7RPJ
-- AWS::SNS::Topic CREATE_COMPLETE CPUAlarmHigh 2011-06-21T20:29:23Z
-- MyStack-CPUAlarmHigh-POBWQPDJA81F AWS::CloudWatch::Alarm
-- 2d06e36c-ac1d-11e0-a958-f9382b6eb86b.
--
-- See: 'Network.AWS.CloudFormation.ListStackResources'

listStackResources :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'lsrStackName'
    -> State ListStackResources a
    -> Source m ListStackResourcesResponse
listStackResources p1 s =
    paginate $ (mkListStackResources p1) &~ s

listStackResourcesCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'lsrStackName'
    -> State ListStackResources a
    -> Source m (Either ServiceEr ListStackResourcesResponse)
listStackResourcesCatch p1 s =
    paginateCatch $ (mkListStackResources p1) &~ s

-- $ListStacks
-- Returns the summary information for stacks whose status matches the
-- specified StackStatusFilter. Summary information for stacks that have been
-- deleted is kept for 90 days after the stack is deleted. If no
-- StackStatusFilter is specified, summary information for all stacks is
-- returned (including existing stacks and stacks that have been deleted).
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=ListStacks
-- &StackStatusFilter.member.1=CREATE_IN_PROGRESS
-- &StackStatusFilter.member.2=DELETE_COMPLETE &Version=2010-05-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature]
-- arn:aws:cloudformation:us-east-1:1234567:stack/TestCreate1/aaaaa
-- CREATE_IN_PROGRESS vpc1 2011-05-23T15:47:44Z Creates one EC2 instance and a
-- load balancer.
-- arn:aws:cloudformation:us-east-1:1234567:stack/TestDelete2/bbbbb
-- DELETE_COMPLETE 2011-03-10T16:20:51Z WP1 2011-03-05T19:57:58Z A simple
-- basic Cloudformation Template.
--
-- See: 'Network.AWS.CloudFormation.ListStacks'

listStacks :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              )
    => State ListStacks a
    -> Source m ListStacksResponse
listStacks s =
    paginate (mkListStacks &~ s)

listStacksCatch :: ( MonadCatch m
                   , MonadResource m
                   , MonadReader Env m
                   )
    => State ListStacks a
    -> Source m (Either ServiceEr ListStacksResponse)
listStacksCatch s =
    paginateCatch (mkListStacks &~ s)

-- $SetStackPolicy
-- Sets a stack policy for a specified stack.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=SetStackPolicy
-- &StackName=MyStack &StackPolicyBody=[Stack Policy Document]
-- &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature].
--
-- See: 'Network.AWS.CloudFormation.SetStackPolicy'

setStackPolicy :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'sspStackName'
    -> State SetStackPolicy a
    -> m SetStackPolicyResponse
setStackPolicy p1 s =
    send $ (mkSetStackPolicy p1) &~ s

setStackPolicyCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'sspStackName'
    -> State SetStackPolicy a
    -> m (Either ServiceEr SetStackPolicyResponse)
setStackPolicyCatch p1 s =
    sendCatch $ (mkSetStackPolicy p1) &~ s

-- $UpdateStack
-- Updates a stack as specified in the template. After the call completes
-- successfully, the stack update starts. You can check the status of the
-- stack via the DescribeStacks action. To get a copy of the template for an
-- existing stack, you can use the GetTemplate action. Tags that were
-- associated with this stack during creation time will still be associated
-- with the stack after an UpdateStack operation. For more information about
-- creating an update template, updating a stack, and monitoring the progress
-- of the update, see Updating a Stack.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=UpdateStack
-- &StackName=MyStack &TemplateBody=[Template Document]
-- &Parameters.member.1.ParameterKey=AvailabilityZone
-- &Parameters.member.1.ParameterValue=us-east-1a &Version=2010-05-15
-- &SignatureVersion=2 &Timestamp=2010-07-27T22%3A26%3A28.000Z
-- &AWSAccessKeyId=[AWS Access KeyID] &Signature=[Signature]
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83.
-- 
--
-- See: 'Network.AWS.CloudFormation.UpdateStack'

updateStack :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               )
    => Text -- ^ 'usStackName'
    -> State UpdateStack a
    -> m UpdateStackResponse
updateStack p1 s =
    send $ (mkUpdateStack p1) &~ s

updateStackCatch :: ( MonadCatch m
                    , MonadResource m
                    , MonadReader Env m
                    )
    => Text -- ^ 'usStackName'
    -> State UpdateStack a
    -> m (Either ServiceEr UpdateStackResponse)
updateStackCatch p1 s =
    sendCatch $ (mkUpdateStack p1) &~ s

-- $ValidateTemplate
-- Validates a specified template.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=ValidateTemplate
-- &TemplateBody=http://myTemplateRepository/TemplateOne.template
-- &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature] false InstanceType Type of instance to launch
-- m1.small false WebServerPort The TCP port for the Web Server 8888 false
-- KeyName Name of an existing EC2 KeyPair to enable SSH access into the
-- server 0be7b6e8-e4a0-11e0-a5bd-9f8d5a7dbc91.
--
-- See: 'Network.AWS.CloudFormation.ValidateTemplate'

validateTemplate :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => State ValidateTemplate a
    -> m ValidateTemplateResponse
validateTemplate s =
    send (mkValidateTemplate &~ s)

validateTemplateCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => State ValidateTemplate a
    -> m (Either ServiceEr ValidateTemplateResponse)
validateTemplateCatch s =
    sendCatch (mkValidateTemplate &~ s)
