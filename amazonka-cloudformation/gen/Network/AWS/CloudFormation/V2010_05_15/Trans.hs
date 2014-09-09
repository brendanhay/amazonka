{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.CloudFormation.V2010_05_15.Trans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS CloudFormation gives developers and systems administrators an easy way
-- to create and manage a collection of related AWS resources, provisioning
-- and updating them in an orderly and predictable fashion. You can use AWS
-- CloudFormation’s sample templates or create your own templates to describe
-- the AWS resources, and any associated dependencies or runtime parameters,
-- required to run your application. You don’t need to figure out the order
-- for provisioning AWS services or the subtleties of making those
-- dependencies work. CloudFormation takes care of this for you. After the AWS
-- resources are deployed, you can modify and update them in a controlled and
-- predictable way, in effect applying version control to your AWS
-- infrastructure the same way you do with your software.
module Network.AWS.CloudFormation.V2010_05_15.Trans
    (
    -- * CancelUpdateStack
      cancelUpdateStack
    -- * CreateStack
    , createStack
    -- * DeleteStack
    , deleteStack
    -- * DescribeStackEvents
    , describeStackEvents
    -- * DescribeStackResource
    , describeStackResource
    -- * DescribeStackResources
    , describeStackResources
    -- * DescribeStacks
    , describeStacks
    -- * EstimateTemplateCost
    , estimateTemplateCost
    -- * GetStackPolicy
    , getStackPolicy
    -- * GetTemplate
    , getTemplate
    -- * ListStackResources
    , listStackResources
    -- * ListStacks
    , listStacks
    -- * SetStackPolicy
    , setStackPolicy
    -- * UpdateStack
    , updateStack
    -- * ValidateTemplate
    , validateTemplate

    -- * Re-exported
    , module Network.AWS.CloudFormation.V2010_05_15
    ) where

import Control.Monad.Trans.AWS
import Network.AWS.Prelude
import Network.AWS.CloudFormation.V2010_05_15

-- | Cancels an update on the specified stack. If the call completes
-- successfully, the stack will roll back the update and revert to the
-- previous stack configuration. Only stacks that are in the
-- UPDATE_IN_PROGRESS state can be canceled.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=CancelUpdateStack
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature].
--
-- See: 'Network.AWS.CloudFormation.V2010_05_15.CancelUpdateStack'
cancelUpdateStack :: ( MonadCatch m
                     , MonadResource m
                     , MonadError Error m
                     , MonadReader Env m
                     , AWSRequest a
                     )
                  => Text -- ^ 'cusStackName'
                  -> State CancelUpdateStack a
                  -> m CancelUpdateStackResponse
cancelUpdateStack p1 s =
    send $ (mkCancelUpdateStack p1) &~ s

-- | Creates a stack as specified in the template. After the call completes
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
-- See: 'Network.AWS.CloudFormation.V2010_05_15.CreateStack'
createStack :: ( MonadCatch m
               , MonadResource m
               , MonadError Error m
               , MonadReader Env m
               , AWSRequest a
               )
            => Text -- ^ 'csStackName'
            -> State CreateStack a
            -> m CreateStackResponse
createStack p1 s =
    send $ (mkCreateStack p1) &~ s

-- | Deletes a specified stack. Once the call completes successfully, stack
-- deletion starts. Deleted stacks do not show up in the DescribeStacks API if
-- the deletion has been completed successfully.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=DeleteStack
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature].
--
-- See: 'Network.AWS.CloudFormation.V2010_05_15.DeleteStack'
deleteStack :: ( MonadCatch m
               , MonadResource m
               , MonadError Error m
               , MonadReader Env m
               , AWSRequest a
               )
            => Text -- ^ 'dsStackName'
            -> State DeleteStack a
            -> m DeleteStackResponse
deleteStack p1 s =
    send $ (mkDeleteStack p1) &~ s

-- | Returns all stack related events for a specified stack. For more
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
-- See: 'Network.AWS.CloudFormation.V2010_05_15.DescribeStackEvents'
describeStackEvents :: ( MonadCatch m
                       , MonadResource m
                       , MonadError Error m
                       , MonadReader Env (ResumableSource m)
                       , AWSPager a
                       )
                    => Text -- ^ 'dseStackName'
                    -> State DescribeStackEvents a
                    -> ResumableSource m DescribeStackEventsResponse
describeStackEvents p1 s =
    paginate $ (mkDescribeStackEvents p1) &~ s

-- | Returns a description of the specified resource in the specified stack. For
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
-- See: 'Network.AWS.CloudFormation.V2010_05_15.DescribeStackResource'
describeStackResource :: ( MonadCatch m
                         , MonadResource m
                         , MonadError Error m
                         , MonadReader Env m
                         , AWSRequest a
                         )
                      => Text -- ^ 'dsrStackName'
                      -> Text -- ^ 'dsrLogicalResourceId'
                      -> State DescribeStackResource a
                      -> m DescribeStackResourceResponse
describeStackResource p1 p2 s =
    send $ (mkDescribeStackResource p1 p2) &~ s

-- | Returns AWS resource descriptions for running and deleted stacks. If
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
-- See: 'Network.AWS.CloudFormation.V2010_05_15.DescribeStackResources'
describeStackResources :: ( MonadCatch m
                          , MonadResource m
                          , MonadError Error m
                          , MonadReader Env m
                          , AWSRequest a
                          )
                       => State DescribeStackResources a
                       -> m DescribeStackResourcesResponse
describeStackResources s =
    send (mkDescribeStackResources &~ s)

-- | Returns the description for the specified stack; if no stack name was
-- specified, then it returns the description for all the stacks created.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=DescribeStacks
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature] MyStack
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83
-- 2010-07-27T22:28:28Z CREATE_COMPLETE false StartPage
-- http://my-load-balancer.amazonaws.com:80/index.html.
--
-- See: 'Network.AWS.CloudFormation.V2010_05_15.DescribeStacks'
describeStacks :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env (ResumableSource m)
                  , AWSPager a
                  )
               => State DescribeStacks a
               -> ResumableSource m DescribeStacksResponse
describeStacks s =
    paginate (mkDescribeStacks &~ s)

-- | Returns the estimated monthly cost of a template. The return value is an
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
-- See: 'Network.AWS.CloudFormation.V2010_05_15.EstimateTemplateCost'
estimateTemplateCost :: ( MonadCatch m
                        , MonadResource m
                        , MonadError Error m
                        , MonadReader Env m
                        , AWSRequest a
                        )
                     => State EstimateTemplateCost a
                     -> m EstimateTemplateCostResponse
estimateTemplateCost s =
    send (mkEstimateTemplateCost &~ s)

-- | Returns the stack policy for a specified stack. If a stack doesn't have a
-- policy, a null value is returned.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=GetStackPolicy
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature] "{ "Statement" : [ { "Effect" : "Deny", "Action" :
-- "Update:*", "Principal" : "*", "Resource" :
-- "LogicalResourceId/ProductionDatabase" }, { "Effect" : "Allow", "Action" :
-- "Update:*", "Principal" : "*", "Resource" : "*" } ] }.
--
-- See: 'Network.AWS.CloudFormation.V2010_05_15.GetStackPolicy'
getStackPolicy :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env m
                  , AWSRequest a
                  )
               => Text -- ^ 'gspStackName'
               -> State GetStackPolicy a
               -> m GetStackPolicyResponse
getStackPolicy p1 s =
    send $ (mkGetStackPolicy p1) &~ s

-- | Returns the template body for a specified stack. You can get the template
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
-- See: 'Network.AWS.CloudFormation.V2010_05_15.GetTemplate'
getTemplate :: ( MonadCatch m
               , MonadResource m
               , MonadError Error m
               , MonadReader Env m
               , AWSRequest a
               )
            => Text -- ^ 'gtStackName'
            -> State GetTemplate a
            -> m GetTemplateResponse
getTemplate p1 s =
    send $ (mkGetTemplate p1) &~ s

-- | Returns descriptions of all resources of the specified stack. For deleted
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
-- See: 'Network.AWS.CloudFormation.V2010_05_15.ListStackResources'
listStackResources :: ( MonadCatch m
                      , MonadResource m
                      , MonadError Error m
                      , MonadReader Env (ResumableSource m)
                      , AWSPager a
                      )
                   => Text -- ^ 'lsrStackName'
                   -> State ListStackResources a
                   -> ResumableSource m ListStackResourcesResponse
listStackResources p1 s =
    paginate $ (mkListStackResources p1) &~ s

-- | Returns the summary information for stacks whose status matches the
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
-- See: 'Network.AWS.CloudFormation.V2010_05_15.ListStacks'
listStacks :: ( MonadCatch m
              , MonadResource m
              , MonadError Error m
              , MonadReader Env (ResumableSource m)
              , AWSPager a
              )
           => State ListStacks a
           -> ResumableSource m ListStacksResponse
listStacks s =
    paginate (mkListStacks &~ s)

-- | Sets a stack policy for a specified stack.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=SetStackPolicy
-- &StackName=MyStack &StackPolicyBody=[Stack Policy Document]
-- &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature].
--
-- See: 'Network.AWS.CloudFormation.V2010_05_15.SetStackPolicy'
setStackPolicy :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env m
                  , AWSRequest a
                  )
               => Text -- ^ 'sspStackName'
               -> State SetStackPolicy a
               -> m SetStackPolicyResponse
setStackPolicy p1 s =
    send $ (mkSetStackPolicy p1) &~ s

-- | Updates a stack as specified in the template. After the call completes
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
-- See: 'Network.AWS.CloudFormation.V2010_05_15.UpdateStack'
updateStack :: ( MonadCatch m
               , MonadResource m
               , MonadError Error m
               , MonadReader Env m
               , AWSRequest a
               )
            => Text -- ^ 'usStackName'
            -> State UpdateStack a
            -> m UpdateStackResponse
updateStack p1 s =
    send $ (mkUpdateStack p1) &~ s

-- | Validates a specified template.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=ValidateTemplate
-- &TemplateBody=http://myTemplateRepository/TemplateOne.template
-- &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature] false InstanceType Type of instance to launch
-- m1.small false WebServerPort The TCP port for the Web Server 8888 false
-- KeyName Name of an existing EC2 KeyPair to enable SSH access into the
-- server 0be7b6e8-e4a0-11e0-a5bd-9f8d5a7dbc91.
--
-- See: 'Network.AWS.CloudFormation.V2010_05_15.ValidateTemplate'
validateTemplate :: ( MonadCatch m
                    , MonadResource m
                    , MonadError Error m
                    , MonadReader Env m
                    , AWSRequest a
                    )
                 => State ValidateTemplate a
                 -> m ValidateTemplateResponse
validateTemplate s =
    send (mkValidateTemplate &~ s)
