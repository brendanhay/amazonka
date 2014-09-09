{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.SWF.V2012_01_25.Trans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Simple Workflow Service (Amazon SWF) makes it easy to build
-- applications that coordinate work across distributed components. In Amazon
-- SWF, a task represents a logical unit of work that is performed by a
-- component of your application. Coordinating tasks across the application
-- involves managing intertask dependencies, scheduling, and concurrency in
-- accordance with the logical flow of the application. Amazon SWF gives you
-- full control over implementing tasks and coordinating them without worrying
-- about underlying complexities such as tracking their progress and
-- maintaining their state.
module Network.AWS.SWF.V2012_01_25.Trans
    (
    -- * CountClosedWorkflowExecutions
      countClosedWorkflowExecutions
    -- * CountOpenWorkflowExecutions
    , countOpenWorkflowExecutions
    -- * CountPendingActivityTasks
    , countPendingActivityTasks
    -- * CountPendingDecisionTasks
    , countPendingDecisionTasks
    -- * DeprecateActivityType
    , deprecateActivityType
    -- * DeprecateDomain
    , deprecateDomain
    -- * DeprecateWorkflowType
    , deprecateWorkflowType
    -- * DescribeActivityType
    , describeActivityType
    -- * DescribeDomain
    , describeDomain
    -- * DescribeWorkflowExecution
    , describeWorkflowExecution
    -- * DescribeWorkflowType
    , describeWorkflowType
    -- * GetWorkflowExecutionHistory
    , getWorkflowExecutionHistory
    -- * ListActivityTypes
    , listActivityTypes
    -- * ListClosedWorkflowExecutions
    , listClosedWorkflowExecutions
    -- * ListDomains
    , listDomains
    -- * ListOpenWorkflowExecutions
    , listOpenWorkflowExecutions
    -- * ListWorkflowTypes
    , listWorkflowTypes
    -- * PollForActivityTask
    , pollForActivityTask
    -- * PollForDecisionTask
    , pollForDecisionTask
    -- * RecordActivityTaskHeartbeat
    , recordActivityTaskHeartbeat
    -- * RegisterActivityType
    , registerActivityType
    -- * RegisterDomain
    , registerDomain
    -- * RegisterWorkflowType
    , registerWorkflowType
    -- * RequestCancelWorkflowExecution
    , requestCancelWorkflowExecution
    -- * RespondActivityTaskCanceled
    , respondActivityTaskCanceled
    -- * RespondActivityTaskCompleted
    , respondActivityTaskCompleted
    -- * RespondActivityTaskFailed
    , respondActivityTaskFailed
    -- * RespondDecisionTaskCompleted
    , respondDecisionTaskCompleted
    -- * SignalWorkflowExecution
    , signalWorkflowExecution
    -- * StartWorkflowExecution
    , startWorkflowExecution
    -- * TerminateWorkflowExecution
    , terminateWorkflowExecution

    -- * Re-exported
    , module Network.AWS.SWF.V2012_01_25
    ) where

import Control.Monad.Trans.AWS
import Network.AWS.Prelude
import Network.AWS.SWF.V2012_01_25

-- | Returns the number of closed workflow executions within the given domain
-- that meet the specified filtering criteria. This operation is eventually
-- consistent. The results are best effort and may not exactly reflect recent
-- updates and changes. Access Control You can use IAM policies to control
-- this action's access to Amazon SWF resources as follows: Use a Resource
-- element with the domain name to limit the action to only specified domains.
-- Use an Action element to allow or deny permission to call this action.
-- Constrain the following parameters by using a Condition element with the
-- appropriate keys. tagFilter.tag: String constraint. The key is
-- swf:tagFilter.tag. typeFilter.name: String constraint. The key is
-- swf:typeFilter.name. typeFilter.version: String constraint. The key is
-- swf:typeFilter.version. If the caller does not have sufficient permissions
-- to invoke the action, or the parameter values fall outside the specified
-- constraints, the action fails by throwing OperationNotPermitted. For
-- details and example IAM policies, see Using IAM to Manage Access to Amazon
-- SWF Workflows. CountClosedWorkflowExecutions Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 02:42:47 GMT X-Amz-Target:
-- SimpleWorkflowService.CountClosedWorkflowExecutions Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=jFS74utjeATV7vj72CWdLToPCKW0RQse6OEDkafB+SA=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 157 Pragma: no-cache Cache-Control: no-cache { "domain":
-- "867530901", "closeTimeFilter": {"oldestDate": 1325376070, "latestDate":
-- 1356998399}, "closeStatusFilter": {"status": "TIMED_OUT"} } HTTP/1.1 200 OK
-- Content-Length: 29 Content-Type: application/json x-amzn-RequestId:
-- 9bfad387-3f22-11e1-9914-a356b6ea8bdf { "count":3, "truncated":false }.
--
-- See: 'Network.AWS.SWF.V2012_01_25.CountClosedWorkflowExecutions'
countClosedWorkflowExecutions :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadError Error m
                                 , MonadReader Env m
                                 , AWSRequest a
                                 )
                              => Text -- ^ 'ccweDomain'
                              -> State CountClosedWorkflowExecutions a
                              -> m CountClosedWorkflowExecutionsResponse
countClosedWorkflowExecutions p1 s =
    send $ (mkCountClosedWorkflowExecutions p1) &~ s

-- | Returns the number of open workflow executions within the given domain that
-- meet the specified filtering criteria. This operation is eventually
-- consistent. The results are best effort and may not exactly reflect recent
-- updates and changes. Access Control You can use IAM policies to control
-- this action's access to Amazon SWF resources as follows: Use a Resource
-- element with the domain name to limit the action to only specified domains.
-- Use an Action element to allow or deny permission to call this action.
-- Constrain the following parameters by using a Condition element with the
-- appropriate keys. tagFilter.tag: String constraint. The key is
-- swf:tagFilter.tag. typeFilter.name: String constraint. The key is
-- swf:typeFilter.name. typeFilter.version: String constraint. The key is
-- swf:typeFilter.version. If the caller does not have sufficient permissions
-- to invoke the action, or the parameter values fall outside the specified
-- constraints, the action fails by throwing OperationNotPermitted. For
-- details and example IAM policies, see Using IAM to Manage Access to Amazon
-- SWF Workflows. CountOpenWorkflowExecutions Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Sat, 14 Jan 2012 23:13:29 GMT X-Amz-Target:
-- SimpleWorkflowService.CountOpenWorkflowExecutions Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=3v6shiGzWukq4KiX/5HFMIUF/w5qajhW4dp+6AKyOtY=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 150 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "startTimeFilter": {"oldestDate": 1325376070, "latestDate":
-- 1356998399}, "tagFilter": {"tag": "ricoh-the-dog"} } HTTP/1.1 200 OK
-- Content-Length: 29 Content-Type: application/json x-amzn-RequestId:
-- 5ea6789e-3f05-11e1-9e8f-57bb03e21482 {"count":1,"truncated":false}.
--
-- See: 'Network.AWS.SWF.V2012_01_25.CountOpenWorkflowExecutions'
countOpenWorkflowExecutions :: ( MonadCatch m
                               , MonadResource m
                               , MonadError Error m
                               , MonadReader Env m
                               , AWSRequest a
                               )
                            => Text -- ^ 'coweDomain'
                            -> ExecutionTimeFilter -- ^ 'coweStartTimeFilter'
                            -> State CountOpenWorkflowExecutions a
                            -> m CountOpenWorkflowExecutionsResponse
countOpenWorkflowExecutions p1 p2 s =
    send $ (mkCountOpenWorkflowExecutions p1 p2) &~ s

-- | Returns the estimated number of activity tasks in the specified task list.
-- The count returned is an approximation and is not guaranteed to be exact.
-- If you specify a task list that no activity task was ever scheduled in then
-- 0 will be returned. Access Control You can use IAM policies to control this
-- action's access to Amazon SWF resources as follows: Use a Resource element
-- with the domain name to limit the action to only specified domains. Use an
-- Action element to allow or deny permission to call this action. Constrain
-- the taskList.name parameter by using a Condition element with the
-- swf:taskList.name key to allow the action to access only certain task
-- lists. If the caller does not have sufficient permissions to invoke the
-- action, or the parameter values fall outside the specified constraints, the
-- action fails by throwing OperationNotPermitted. For details and example IAM
-- policies, see Using IAM to Manage Access to Amazon SWF Workflows.
-- CountPendingActivityTasks Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 03:29:28 GMT X-Amz-Target:
-- SimpleWorkflowService.CountPendingActivityTasks Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=eCNiyyl5qmP0gGQ0hM8LqeRzxEvVZ0LAjE4oxVzzk9w=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 70 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "taskList": {"name": "specialTaskList"} } HTTP/1.1 200 OK
-- Content-Length: 29 Content-Type: application/json x-amzn-RequestId:
-- 4b977c76-3ff2-11e1-a23a-99d60383ae71 {"count":1,"truncated":false}.
--
-- See: 'Network.AWS.SWF.V2012_01_25.CountPendingActivityTasks'
countPendingActivityTasks :: ( MonadCatch m
                             , MonadResource m
                             , MonadError Error m
                             , MonadReader Env m
                             , AWSRequest a
                             )
                          => Text -- ^ 'cpatDomain'
                          -> TaskList -- ^ 'cpatTaskList'
                          -> State CountPendingActivityTasks a
                          -> m CountPendingActivityTasksResponse
countPendingActivityTasks p1 p2 s =
    send $ (mkCountPendingActivityTasks p1 p2) &~ s

-- | Returns the estimated number of decision tasks in the specified task list.
-- The count returned is an approximation and is not guaranteed to be exact.
-- If you specify a task list that no decision task was ever scheduled in then
-- 0 will be returned. Access Control You can use IAM policies to control this
-- action's access to Amazon SWF resources as follows: Use a Resource element
-- with the domain name to limit the action to only specified domains. Use an
-- Action element to allow or deny permission to call this action. Constrain
-- the taskList.name parameter by using a Condition element with the
-- swf:taskList.name key to allow the action to access only certain task
-- lists. If the caller does not have sufficient permissions to invoke the
-- action, or the parameter values fall outside the specified constraints, the
-- action fails by throwing OperationNotPermitted. For details and example IAM
-- policies, see Using IAM to Manage Access to Amazon SWF Workflows.
-- CountPendingDecisionTasks Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 23:25:57 GMT X-Amz-Target:
-- SimpleWorkflowService.CountPendingDecisionTasks Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=i9tUkWnZBLfn/T6BOymajCtwArAll6Stuh1x2C4dbsE=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 70 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "taskList": {"name": "specialTaskList"} } HTTP/1.1 200 OK
-- Content-Length: 29 Content-Type: application/json x-amzn-RequestId:
-- 4718a364-3fd0-11e1-9914-a356b6ea8bdf {"count": 2, "truncated": false}.
--
-- See: 'Network.AWS.SWF.V2012_01_25.CountPendingDecisionTasks'
countPendingDecisionTasks :: ( MonadCatch m
                             , MonadResource m
                             , MonadError Error m
                             , MonadReader Env m
                             , AWSRequest a
                             )
                          => Text -- ^ 'cpdtDomain'
                          -> TaskList -- ^ 'cpdtTaskList'
                          -> State CountPendingDecisionTasks a
                          -> m CountPendingDecisionTasksResponse
countPendingDecisionTasks p1 p2 s =
    send $ (mkCountPendingDecisionTasks p1 p2) &~ s

-- | Deprecates the specified activity type. After an activity type has been
-- deprecated, you cannot create new tasks of that activity type. Tasks of
-- this type that were scheduled before the type was deprecated will continue
-- to run. This operation is eventually consistent. The results are best
-- effort and may not exactly reflect recent updates and changes. Access
-- Control You can use IAM policies to control this action's access to Amazon
-- SWF resources as follows: Use a Resource element with the domain name to
-- limit the action to only specified domains. Use an Action element to allow
-- or deny permission to call this action. Constrain the following parameters
-- by using a Condition element with the appropriate keys. activityType.name:
-- String constraint. The key is swf:activityType.name. activityType.version:
-- String constraint. The key is swf:activityType.version. If the caller does
-- not have sufficient permissions to invoke the action, or the parameter
-- values fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. DeprecateActivityType Example
-- POST / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 05:01:06 GMT
-- X-Amz-Target: SimpleWorkflowService.DeprecateActivityType Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=iX/mNMtNH6IaSNwfZq9hHOhDlLnp7buuj9tO93kRIrQ=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 95 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "activityType": {"name": "activityVerify", "version": "1.0"} }
-- HTTP/1.1 200 OK Content-Length: 0 Content-Type: application/json
-- x-amzn-RequestId: 191ee17e-3fff-11e1-a23a-99d60383ae71.
--
-- See: 'Network.AWS.SWF.V2012_01_25.DeprecateActivityType'
deprecateActivityType :: ( MonadCatch m
                         , MonadResource m
                         , MonadError Error m
                         , MonadReader Env m
                         , AWSRequest a
                         )
                      => Text -- ^ 'datDomain'
                      -> ActivityType -- ^ 'datActivityType'
                      -> State DeprecateActivityType a
                      -> m DeprecateActivityTypeResponse
deprecateActivityType p1 p2 s =
    send $ (mkDeprecateActivityType p1 p2) &~ s

-- | Deprecates the specified domain. After a domain has been deprecated it
-- cannot be used to create new workflow executions or register new types.
-- However, you can still use visibility actions on this domain. Deprecating a
-- domain also deprecates all activity and workflow types registered in the
-- domain. Executions that were started before the domain was deprecated will
-- continue to run. This operation is eventually consistent. The results are
-- best effort and may not exactly reflect recent updates and changes. Access
-- Control You can use IAM policies to control this action's access to Amazon
-- SWF resources as follows: Use a Resource element with the domain name to
-- limit the action to only specified domains. Use an Action element to allow
-- or deny permission to call this action. You cannot use an IAM policy to
-- constrain this action's parameters. If the caller does not have sufficient
-- permissions to invoke the action, or the parameter values fall outside the
-- specified constraints, the action fails by throwing OperationNotPermitted.
-- For details and example IAM policies, see Using IAM to Manage Access to
-- Amazon SWF Workflows. DeprecateDomain Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 05:07:47 GMT X-Amz-Target:
-- SimpleWorkflowService.DeprecateDomain Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=BkJDtbH9uZvrarqXTkBEYuYHO7PPygRI8ykV29Dz/5M=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 21 Pragma: no-cache Cache-Control: no-cache {"name":
-- "867530901"} HTTP/1.1 200 OK Content-Length: 0 Content-Type:
-- application/json x-amzn-RequestId: 0800c01a-4000-11e1-9914-a356b6ea8bdf.
--
-- See: 'Network.AWS.SWF.V2012_01_25.DeprecateDomain'
deprecateDomain :: ( MonadCatch m
                   , MonadResource m
                   , MonadError Error m
                   , MonadReader Env m
                   , AWSRequest a
                   )
                => Text -- ^ 'ddName'
                -> State DeprecateDomain a
                -> m DeprecateDomainResponse
deprecateDomain p1 s =
    send $ (mkDeprecateDomain p1) &~ s

-- | Deprecates the specified workflow type. After a workflow type has been
-- deprecated, you cannot create new executions of that type. Executions that
-- were started before the type was deprecated will continue to run. A
-- deprecated workflow type may still be used when calling visibility actions.
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes. Access Control You can
-- use IAM policies to control this action's access to Amazon SWF resources as
-- follows: Use a Resource element with the domain name to limit the action to
-- only specified domains. Use an Action element to allow or deny permission
-- to call this action. Constrain the following parameters by using a
-- Condition element with the appropriate keys. workflowType.name: String
-- constraint. The key is swf:workflowType.name. workflowType.version: String
-- constraint. The key is swf:workflowType.version. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. DeprecateWorkflowType Example
-- POST / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 05:04:47 GMT
-- X-Amz-Target: SimpleWorkflowService.DeprecateWorkflowType Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=BGrr1djQvp+YLq3ci2ffpK8KWhZm/PakBL2fFhc3zds=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 102 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "workflowType": {"name": "customerOrderWorkflow", "version":
-- "1.0"} } HTTP/1.1 200 OK Content-Length: 0 Content-Type: application/json
-- x-amzn-RequestId: 9c8d6d3b-3fff-11e1-9e8f-57bb03e21482.
--
-- See: 'Network.AWS.SWF.V2012_01_25.DeprecateWorkflowType'
deprecateWorkflowType :: ( MonadCatch m
                         , MonadResource m
                         , MonadError Error m
                         , MonadReader Env m
                         , AWSRequest a
                         )
                      => Text -- ^ 'dwtDomain'
                      -> WorkflowType -- ^ 'dwtWorkflowType'
                      -> State DeprecateWorkflowType a
                      -> m DeprecateWorkflowTypeResponse
deprecateWorkflowType p1 p2 s =
    send $ (mkDeprecateWorkflowType p1 p2) &~ s

-- | Returns information about the specified activity type. This includes
-- configuration settings provided at registration time as well as other
-- general information about the type. Access Control You can use IAM policies
-- to control this action's access to Amazon SWF resources as follows: Use a
-- Resource element with the domain name to limit the action to only specified
-- domains. Use an Action element to allow or deny permission to call this
-- action. Constrain the following parameters by using a Condition element
-- with the appropriate keys. activityType.name: String constraint. The key is
-- swf:activityType.name. activityType.version: String constraint. The key is
-- swf:activityType.version. If the caller does not have sufficient
-- permissions to invoke the action, or the parameter values fall outside the
-- specified constraints, the action fails by throwing OperationNotPermitted.
-- For details and example IAM policies, see Using IAM to Manage Access to
-- Amazon SWF Workflows. DescribeActivityType Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 03:04:10 GMT X-Amz-Target:
-- SimpleWorkflowService.DescribeActivityType Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=XiGRwOZNLt+ic3VBWvIlRGdcFcRJVSE8J7zyZLU3oXg=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 95 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "activityType": {"name": "activityVerify", "version": "1.0"} }
-- HTTP/1.1 200 OK Content-Length: 387 Content-Type: application/json
-- x-amzn-RequestId: 98d56ff5-3f25-11e1-9b11-7182192d0b57 {"configuration":
-- {"defaultTaskHeartbeatTimeout": "120", "defaultTaskList": {"name":
-- "mainTaskList"}, "defaultTaskScheduleToCloseTimeout": "900",
-- "defaultTaskScheduleToStartTimeout": "300",
-- "defaultTaskStartToCloseTimeout": "600"}, "typeInfo": {"activityType":
-- {"name": "activityVerify", "version": "1.0"}, "creationDate":
-- 1326586446.471, "description": "Verify the customer credit", "status":
-- "REGISTERED"} }.
--
-- See: 'Network.AWS.SWF.V2012_01_25.DescribeActivityType'
describeActivityType :: ( MonadCatch m
                        , MonadResource m
                        , MonadError Error m
                        , MonadReader Env m
                        , AWSRequest a
                        )
                     => Text -- ^ 'dat1Domain'
                     -> ActivityType -- ^ 'dat1ActivityType'
                     -> State DescribeActivityType a
                     -> m DescribeActivityTypeResponse
describeActivityType p1 p2 s =
    send $ (mkDescribeActivityType p1 p2) &~ s

-- | Returns information about the specified domain including description and
-- status. Access Control You can use IAM policies to control this action's
-- access to Amazon SWF resources as follows: Use a Resource element with the
-- domain name to limit the action to only specified domains. Use an Action
-- element to allow or deny permission to call this action. You cannot use an
-- IAM policy to constrain this action's parameters. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. DescribeDomain Example POST /
-- HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 03:13:33 GMT
-- X-Amz-Target: SimpleWorkflowService.DescribeDomain Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=IFJtq3M366CHqMlTpyqYqd9z0ChCoKDC5SCJBsLifu4=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 21 Pragma: no-cache Cache-Control: no-cache {"name":
-- "867530901"} HTTP/1.1 200 OK Content-Length: 137 Content-Type:
-- application/json x-amzn-RequestId: e86a6779-3f26-11e1-9a27-0760db01a4a8
-- {"configuration": {"workflowExecutionRetentionPeriodInDays": "60"},
-- "domainInfo": {"description": "music", "name": "867530901", "status":
-- "REGISTERED"} }.
--
-- See: 'Network.AWS.SWF.V2012_01_25.DescribeDomain'
describeDomain :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env m
                  , AWSRequest a
                  )
               => Text -- ^ 'dd1Name'
               -> State DescribeDomain a
               -> m DescribeDomainResponse
describeDomain p1 s =
    send $ (mkDescribeDomain p1) &~ s

-- | Returns information about the specified workflow execution including its
-- type and some statistics. This operation is eventually consistent. The
-- results are best effort and may not exactly reflect recent updates and
-- changes. Access Control You can use IAM policies to control this action's
-- access to Amazon SWF resources as follows: Use a Resource element with the
-- domain name to limit the action to only specified domains. Use an Action
-- element to allow or deny permission to call this action. You cannot use an
-- IAM policy to constrain this action's parameters. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. DescribeWorkflowExecution Example
-- POST / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 02:05:18 GMT
-- X-Amz-Target: SimpleWorkflowService.DescribeWorkflowExecution
-- Content-Encoding: amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=ufQVcSkfUyGPLiS8xbkEBqEc2PmEEE/3Lb9Kr8yozs8=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 127 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "execution": {"workflowId": "20110927-T-1", "runId":
-- "06b8f87a-24b3-40b6-9ceb-9676f28e9493"} } HTTP/1.1 200 OK Content-Length:
-- 577 Content-Type: application/json x-amzn-RequestId:
-- 5f85ef79-3f1d-11e1-9e8f-57bb03e21482 {"executionConfiguration":
-- {"childPolicy": "TERMINATE", "executionStartToCloseTimeout": "3600",
-- "taskList": {"name": "specialTaskList"}, "taskStartToCloseTimeout": "600"},
-- "executionInfo": {"cancelRequested": false, "execution": {"runId":
-- "06b8f87a-24b3-40b6-9ceb-9676f28e9493", "workflowId": "20110927-T-1"},
-- "executionStatus": "OPEN", "startTimestamp": 1326592619.474, "tagList":
-- ["music purchase", "digital", "ricoh-the-dog"], "workflowType": {"name":
-- "customerOrderWorkflow", "version": "1.0"} }, "openCounts":
-- {"openActivityTasks": 0, "openChildWorkflowExecutions": 0,
-- "openDecisionTasks": 1, "openTimers": 0} }.
--
-- See: 'Network.AWS.SWF.V2012_01_25.DescribeWorkflowExecution'
describeWorkflowExecution :: ( MonadCatch m
                             , MonadResource m
                             , MonadError Error m
                             , MonadReader Env m
                             , AWSRequest a
                             )
                          => Text -- ^ 'dweDomain'
                          -> WorkflowExecution -- ^ 'dweExecution'
                          -> State DescribeWorkflowExecution a
                          -> m DescribeWorkflowExecutionResponse
describeWorkflowExecution p1 p2 s =
    send $ (mkDescribeWorkflowExecution p1 p2) &~ s

-- | Returns information about the specified workflow type. This includes
-- configuration settings specified when the type was registered and other
-- information such as creation date, current status, etc. Access Control You
-- can use IAM policies to control this action's access to Amazon SWF
-- resources as follows: Use a Resource element with the domain name to limit
-- the action to only specified domains. Use an Action element to allow or
-- deny permission to call this action. Constrain the following parameters by
-- using a Condition element with the appropriate keys. workflowType.name:
-- String constraint. The key is swf:workflowType.name. workflowType.version:
-- String constraint. The key is swf:workflowType.version. If the caller does
-- not have sufficient permissions to invoke the action, or the parameter
-- values fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. DescribeWorkflowType Example POST
-- / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 22:40:40 GMT
-- X-Amz-Target: SimpleWorkflowService.DescribeWorkflowType Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=iGt8t83OmrURqu0pKYbcW6mNdjXbFomevCBPUPQEbaM=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 102 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "workflowType": {"name": "customerOrderWorkflow", "version":
-- "1.0"} } HTTP/1.1 200 OK Content-Length: 348 Content-Type: application/json
-- x-amzn-RequestId: f35a8e7f-3fc9-11e1-a23a-99d60383ae71 {"configuration":
-- {"defaultChildPolicy": "TERMINATE", "defaultExecutionStartToCloseTimeout":
-- "3600", "defaultTaskList": {"name": "mainTaskList"},
-- "defaultTaskStartToCloseTimeout": "600"}, "typeInfo": {"creationDate":
-- 1326481174.027, "description": "Handle customer orders", "status":
-- "REGISTERED", "workflowType": {"name": "customerOrderWorkflow", "version":
-- "1.0"} } }.
--
-- See: 'Network.AWS.SWF.V2012_01_25.DescribeWorkflowType'
describeWorkflowType :: ( MonadCatch m
                        , MonadResource m
                        , MonadError Error m
                        , MonadReader Env m
                        , AWSRequest a
                        )
                     => Text -- ^ 'dwt1Domain'
                     -> WorkflowType -- ^ 'dwt1WorkflowType'
                     -> State DescribeWorkflowType a
                     -> m DescribeWorkflowTypeResponse
describeWorkflowType p1 p2 s =
    send $ (mkDescribeWorkflowType p1 p2) &~ s

-- | Returns the history of the specified workflow execution. The results may be
-- split into multiple pages. To retrieve subsequent pages, make the call
-- again using the nextPageToken returned by the initial call. This operation
-- is eventually consistent. The results are best effort and may not exactly
-- reflect recent updates and changes. Access Control You can use IAM policies
-- to control this action's access to Amazon SWF resources as follows: Use a
-- Resource element with the domain name to limit the action to only specified
-- domains. Use an Action element to allow or deny permission to call this
-- action. You cannot use an IAM policy to constrain this action's parameters.
-- If the caller does not have sufficient permissions to invoke the action, or
-- the parameter values fall outside the specified constraints, the action
-- fails by throwing OperationNotPermitted. For details and example IAM
-- policies, see Using IAM to Manage Access to Amazon SWF Workflows.
-- GetWorkflowExecutionHistory Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 03:44:00 GMT X-Amz-Target:
-- SimpleWorkflowService.GetWorkflowExecutionHistory Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=90GENeUWJbEAMWuVI0dcWatHjltMWddXfLjl0MbNOzM=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 175 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "execution": {"workflowId": "20110927-T-1", "runId":
-- "d29e60b5-fa71-4276-a4be-948b0adcd20b"}, "maximumPageSize": 10,
-- "reverseOrder": true} HTTP/1.1 200 OK Content-Length: 2942 Content-Type:
-- application/json x-amzn-RequestId: 5385723f-3ff4-11e1-b118-3bfa5e8e7fc3
-- {"events": [ {"eventId": 11, "eventTimestamp": 1326671603.102, "eventType":
-- "WorkflowExecutionTimedOut", "workflowExecutionTimedOutEventAttributes":
-- {"childPolicy": "TERMINATE", "timeoutType": "START_TO_CLOSE"} },
-- {"decisionTaskScheduledEventAttributes": {"startToCloseTimeout": "600",
-- "taskList": {"name": "specialTaskList"} }, "eventId": 10, "eventTimestamp":
-- 1326670566.124, "eventType": "DecisionTaskScheduled"},
-- {"activityTaskTimedOutEventAttributes": {"latestHeartbeatRecordedEventId":
-- 0, "scheduledEventId": 8, "startedEventId": 0, "timeoutType":
-- "SCHEDULE_TO_START"}, "eventId": 9, "eventTimestamp": 1326670566.124,
-- "eventType": "ActivityTaskTimedOut"},
-- {"activityTaskScheduledEventAttributes": {"activityId": "verification-27",
-- "activityType": {"name": "activityVerify", "version": "1.0"}, "control":
-- "digital music", "decisionTaskCompletedEventId": 7, "heartbeatTimeout":
-- "120", "input": "5634-0056-4367-0923,12/12,437", "scheduleToCloseTimeout":
-- "900", "scheduleToStartTimeout": "300", "startToCloseTimeout": "600",
-- "taskList": {"name": "specialTaskList"} }, "eventId": 8, "eventTimestamp":
-- 1326670266.115, "eventType": "ActivityTaskScheduled"},
-- {"decisionTaskCompletedEventAttributes": {"executionContext": "Black
-- Friday", "scheduledEventId": 5, "startedEventId": 6}, "eventId": 7,
-- "eventTimestamp": 1326670266.103, "eventType": "DecisionTaskCompleted"},
-- {"decisionTaskStartedEventAttributes": {"identity": "Decider01",
-- "scheduledEventId": 5}, "eventId": 6, "eventTimestamp": 1326670161.497,
-- "eventType": "DecisionTaskStarted"},
-- {"decisionTaskScheduledEventAttributes": {"startToCloseTimeout": "600",
-- "taskList": {"name": "specialTaskList"} }, "eventId": 5, "eventTimestamp":
-- 1326668752.66, "eventType": "DecisionTaskScheduled"},
-- {"decisionTaskTimedOutEventAttributes": {"scheduledEventId": 2,
-- "startedEventId": 3, "timeoutType": "START_TO_CLOSE"}, "eventId": 4,
-- "eventTimestamp": 1326668752.66, "eventType": "DecisionTaskTimedOut"},
-- {"decisionTaskStartedEventAttributes": {"identity": "Decider01",
-- "scheduledEventId": 2}, "eventId": 3, "eventTimestamp": 1326668152.648,
-- "eventType": "DecisionTaskStarted"},
-- {"decisionTaskScheduledEventAttributes": {"startToCloseTimeout": "600",
-- "taskList": {"name": "specialTaskList"} }, "eventId": 2, "eventTimestamp":
-- 1326668003.094, "eventType": "DecisionTaskScheduled"} ], "nextPageToken":
-- "AAAAKgAAAAEAAAAAAAAAATeTvAyvqlQz34ctbGhM5nglWmjzk0hGuHf0g4EO4CblQFku70ukjPgrAHy7Tnp7FaZ0okP8EEWnkfg8gi3Fqy/WVrXyxQaa525D31cIq1owXK21CKR6SQ0Job87G8SHvvqvP7yjLGHlHrRGZUCbJgeEuV4Rp/yW+vKhc8dJ54x7wvpQMwZ+ssG6stTyX26vu1gIDuspk13UrDZa4TbLOFdM0aAocHe3xklKMtD/B4ithem6BWm6CBl/UF7lMfNccwUYEityp1Kht/YrcD9zbJkt1FSt4Y6pgt0njAh4FKRO9nyRyvLmbvgtQXEIQz8hdbjwj3xE1+9ocYwXOCAhVkRsh3OD6F8KHilKfdwg4Xz1jtLXOh4lsCecNb8dS7J9hbRErRbw3rh1Sv415U2Ye23OEfF4Jv7JznpmEyzuq8d2bMyOLjAInQVFK4t1tPo5FAhzdICCXBhRq6Wkt++W9sRQXqqX/HTX5kNomHySZloylPuY5gL5zRj39frInfZk4EXWHwrI+18+erGIHO4nBQpMzO64dMP+A/KtVGCn59rAMmilD6wEE9rH8RuZ03Wkvm9yrJvjrI8/6358n8TMB8OcHoqILkMCAXYiIppnFlm+NWXVqxalHLKOrrNzEZM6qsz3Qj3HV1cpy9P7fnS9QAxrgsAYBoDmdOaFkS3ktAkRa0Sle8STfHi4zKbfIGS7rg=="}.
-- 
--
-- See: 'Network.AWS.SWF.V2012_01_25.GetWorkflowExecutionHistory'
getWorkflowExecutionHistory :: ( MonadCatch m
                               , MonadResource m
                               , MonadError Error m
                               , MonadReader Env (ResumableSource m)
                               , AWSPager a
                               )
                            => Text -- ^ 'gwehDomain'
                            -> WorkflowExecution -- ^ 'gwehExecution'
                            -> State GetWorkflowExecutionHistory a
                            -> ResumableSource m GetWorkflowExecutionHistoryResponse
getWorkflowExecutionHistory p1 p2 s =
    paginate $ (mkGetWorkflowExecutionHistory p1 p2) &~ s

-- | Returns information about all activities registered in the specified domain
-- that match the specified name and registration status. The result includes
-- information like creation date, current status of the activity, etc. The
-- results may be split into multiple pages. To retrieve subsequent pages,
-- make the call again using the nextPageToken returned by the initial call.
-- Access Control You can use IAM policies to control this action's access to
-- Amazon SWF resources as follows: Use a Resource element with the domain
-- name to limit the action to only specified domains. Use an Action element
-- to allow or deny permission to call this action. You cannot use an IAM
-- policy to constrain this action's parameters. If the caller does not have
-- sufficient permissions to invoke the action, or the parameter values fall
-- outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. ListActivityTypes Example
-- {"domain": "867530901", "registrationStatus": "REGISTERED",
-- "maximumPageSize": 50, "reverseOrder": false} HTTP/1.1 200 OK
-- Content-Length: 171 Content-Type: application/json x-amzn-RequestId:
-- 11b6fbeb-3f25-11e1-9e8f-57bb03e21482 {"typeInfos": [ {"activityType":
-- {"name": "activityVerify", "version": "1.0"}, "creationDate":
-- 1326586446.471, "description": "Verify the customer credit", "status":
-- "REGISTERED"} ] }.
--
-- See: 'Network.AWS.SWF.V2012_01_25.ListActivityTypes'
listActivityTypes :: ( MonadCatch m
                     , MonadResource m
                     , MonadError Error m
                     , MonadReader Env (ResumableSource m)
                     , AWSPager a
                     )
                  => Text -- ^ 'latDomain'
                  -> RegistrationStatus -- ^ 'latRegistrationStatus'
                  -> State ListActivityTypes a
                  -> ResumableSource m ListActivityTypesResponse
listActivityTypes p1 p3 s =
    paginate $ (mkListActivityTypes p1 p3) &~ s

-- | Returns a list of closed workflow executions in the specified domain that
-- meet the filtering criteria. The results may be split into multiple pages.
-- To retrieve subsequent pages, make the call again using the nextPageToken
-- returned by the initial call. This operation is eventually consistent. The
-- results are best effort and may not exactly reflect recent updates and
-- changes. Access Control You can use IAM policies to control this action's
-- access to Amazon SWF resources as follows: Use a Resource element with the
-- domain name to limit the action to only specified domains. Use an Action
-- element to allow or deny permission to call this action. Constrain the
-- following parameters by using a Condition element with the appropriate
-- keys. tagFilter.tag: String constraint. The key is swf:tagFilter.tag.
-- typeFilter.name: String constraint. The key is swf:typeFilter.name.
-- typeFilter.version: String constraint. The key is swf:typeFilter.version.
-- If the caller does not have sufficient permissions to invoke the action, or
-- the parameter values fall outside the specified constraints, the action
-- fails by throwing OperationNotPermitted. For details and example IAM
-- policies, see Using IAM to Manage Access to Amazon SWF Workflows.
-- ListClosedWorkflowExecutions Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 02:51:01 GMT X-Amz-Target:
-- SimpleWorkflowService.ListClosedWorkflowExecutions Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=WY9jGbf5E3F9smGJHANhEXz9VL+1oGVgNL0/o7cBxQw=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 150 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "closeTimeFilter": {"oldestDate": 1325376070, "latestDate":
-- 1356998399}, "tagFilter": {"tag": "ricoh-the-dog"} } HTTP/1.1 200 OK
-- Content-Length: 1084 Content-Type: application/json x-amzn-RequestId:
-- c28b4df4-3f23-11e1-9e8f-57bb03e21482 {"executionInfos": [
-- {"cancelRequested": false, "closeStatus": "TIMED_OUT", "closeTimestamp":
-- 1326590754.654, "execution": {"runId":
-- "c724e07a-b966-441f-a1c0-4831acbda1cd", "workflowId": "20110927-T-1"},
-- "executionStatus": "CLOSED", "startTimestamp": 1326587154.626, "tagList":
-- ["music purchase", "digital", "ricoh-the-dog"], "workflowType": {"name":
-- "customerOrderWorkflow", "version": "1.0"} }, {"cancelRequested": false,
-- "closeStatus": "TIMED_OUT", "closeTimestamp": 1326586831.628, "execution":
-- {"runId": "f5ebbac6-941c-4342-ad69-dfd2f8be6689", "workflowId":
-- "20110927-T-1"}, "executionStatus": "CLOSED", "startTimestamp":
-- 1326585031.619, "tagList": ["music purchase", "digital", "ricoh-the-dog"],
-- "workflowType": {"name": "customerOrderWorkflow", "version": "1.0"} },
-- {"cancelRequested": false, "closeStatus": "TIMED_OUT", "closeTimestamp":
-- 1326582914.031, "execution": {"runId":
-- "1e536162-f1ea-48b0-85f3-aade88eef2f7", "workflowId": "20110927-T-1"},
-- "executionStatus": "CLOSED", "startTimestamp": 1326581114.02, "tagList":
-- ["music purchase", "digital", "ricoh-the-dog"], "workflowType": {"name":
-- "customerOrderWorkflow", "version": "1.0"} } ] }.
--
-- See: 'Network.AWS.SWF.V2012_01_25.ListClosedWorkflowExecutions'
listClosedWorkflowExecutions :: ( MonadCatch m
                                , MonadResource m
                                , MonadError Error m
                                , MonadReader Env (ResumableSource m)
                                , AWSPager a
                                )
                             => Text -- ^ 'lcweDomain'
                             -> State ListClosedWorkflowExecutions a
                             -> ResumableSource m ListClosedWorkflowExecutionsResponse
listClosedWorkflowExecutions p1 s =
    paginate $ (mkListClosedWorkflowExecutions p1) &~ s

-- | Returns the list of domains registered in the account. The results may be
-- split into multiple pages. To retrieve subsequent pages, make the call
-- again using the nextPageToken returned by the initial call. This operation
-- is eventually consistent. The results are best effort and may not exactly
-- reflect recent updates and changes. Access Control You can use IAM policies
-- to control this action's access to Amazon SWF resources as follows: Use a
-- Resource element with the domain name to limit the action to only specified
-- domains. The element must be set to arn:aws:swf::AccountID:domain/*", where
-- “AccountID" is the account ID, with no dashes. Use an Action element to
-- allow or deny permission to call this action. You cannot use an IAM policy
-- to constrain this action's parameters. If the caller does not have
-- sufficient permissions to invoke the action, or the parameter values fall
-- outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. ListDomains Example POST /
-- HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 03:09:58 GMT
-- X-Amz-Target: SimpleWorkflowService.ListDomains Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=ZCprC72dUxF9ca3w/tbwKZ8lBQn0jaA4xOJqDF0uqMI=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 86 Pragma: no-cache Cache-Control: no-cache
-- {"registrationStatus": "REGISTERED", "maximumPageSize": 50, "reverseOrder":
-- false} HTTP/1.1 200 OK Content-Length: 568 Content-Type: application/json
-- x-amzn-RequestId: 67e874cc-3f26-11e1-9b11-7182192d0b57 {"domainInfos": [
-- {"description": "music", "name": "867530901", "status": "REGISTERED"},
-- {"description": "music", "name": "867530902", "status": "REGISTERED"},
-- {"description": "", "name": "Demo", "status": "REGISTERED"},
-- {"description": "", "name": "DemoDomain", "status": "REGISTERED"},
-- {"description": "", "name": "Samples", "status": "REGISTERED"},
-- {"description": "", "name": "testDomain2", "status": "REGISTERED"},
-- {"description": "", "name": "testDomain3", "status": "REGISTERED"},
-- {"description": "", "name": "testDomain4", "status": "REGISTERED"},
-- {"description": "", "name": "zsxfvgsxcv", "status": "REGISTERED"} ] }.
--
-- See: 'Network.AWS.SWF.V2012_01_25.ListDomains'
listDomains :: ( MonadCatch m
               , MonadResource m
               , MonadError Error m
               , MonadReader Env (ResumableSource m)
               , AWSPager a
               )
            => RegistrationStatus -- ^ 'ldRegistrationStatus'
            -> State ListDomains a
            -> ResumableSource m ListDomainsResponse
listDomains p2 s =
    paginate $ (mkListDomains p2) &~ s

-- | Returns a list of open workflow executions in the specified domain that
-- meet the filtering criteria. The results may be split into multiple pages.
-- To retrieve subsequent pages, make the call again using the nextPageToken
-- returned by the initial call. This operation is eventually consistent. The
-- results are best effort and may not exactly reflect recent updates and
-- changes. Access Control You can use IAM policies to control this action's
-- access to Amazon SWF resources as follows: Use a Resource element with the
-- domain name to limit the action to only specified domains. Use an Action
-- element to allow or deny permission to call this action. Constrain the
-- following parameters by using a Condition element with the appropriate
-- keys. tagFilter.tag: String constraint. The key is swf:tagFilter.tag.
-- typeFilter.name: String constraint. The key is swf:typeFilter.name.
-- typeFilter.version: String constraint. The key is swf:typeFilter.version.
-- If the caller does not have sufficient permissions to invoke the action, or
-- the parameter values fall outside the specified constraints, the action
-- fails by throwing OperationNotPermitted. For details and example IAM
-- policies, see Using IAM to Manage Access to Amazon SWF Workflows.
-- ListOpenWorkflowExecutions POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Sat, 14 Jan 2012 23:51:04 GMT X-Amz-Target:
-- SimpleWorkflowService.ListOpenWorkflowExecutions Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=4kUhpZUp37PgpeOKHlWTsZi+Pq3Egw4mTkPNiEUgp28=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 151 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "startTimeFilter": {"oldestDate": 1325376070, "latestDate":
-- 1356998399}, "tagFilter": {"tag": "music purchase"} } HTTP/1.1 200 OK
-- Content-Length: 313 Content-Type: application/json x-amzn-RequestId:
-- 9efeff4b-3f0a-11e1-9e8f-57bb03e21482 {"executionInfos": [
-- {"cancelRequested": false, "execution": {"runId":
-- "f5ebbac6-941c-4342-ad69-dfd2f8be6689", "workflowId": "20110927-T-1"},
-- "executionStatus": "OPEN", "startTimestamp": 1326585031.619, "tagList":
-- ["music purchase", "digital", "ricoh-the-dog"], "workflowType": {"name":
-- "customerOrderWorkflow", "version": "1.0"} } ] }.
--
-- See: 'Network.AWS.SWF.V2012_01_25.ListOpenWorkflowExecutions'
listOpenWorkflowExecutions :: ( MonadCatch m
                              , MonadResource m
                              , MonadError Error m
                              , MonadReader Env (ResumableSource m)
                              , AWSPager a
                              )
                           => Text -- ^ 'loweDomain'
                           -> ExecutionTimeFilter -- ^ 'loweStartTimeFilter'
                           -> State ListOpenWorkflowExecutions a
                           -> ResumableSource m ListOpenWorkflowExecutionsResponse
listOpenWorkflowExecutions p1 p2 s =
    paginate $ (mkListOpenWorkflowExecutions p1 p2) &~ s

-- | Returns information about workflow types in the specified domain. The
-- results may be split into multiple pages that can be retrieved by making
-- the call repeatedly. Access Control You can use IAM policies to control
-- this action's access to Amazon SWF resources as follows: Use a Resource
-- element with the domain name to limit the action to only specified domains.
-- Use an Action element to allow or deny permission to call this action. You
-- cannot use an IAM policy to constrain this action's parameters. If the
-- caller does not have sufficient permissions to invoke the action, or the
-- parameter values fall outside the specified constraints, the action fails
-- by throwing OperationNotPermitted. For details and example IAM policies,
-- see Using IAM to Manage Access to Amazon SWF Workflows. ListWorkflowTypes
-- Example POST / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent:
-- Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 22:25:43 GMT
-- X-Amz-Target: SimpleWorkflowService.ListWorkflowTypes Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=uleWQSyVVf0+aG50IoBJG5h0hzxNFNT97Mkn/FSCQ+Q=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 110 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "registrationStatus": "REGISTERED", "maximumPageSize": 50,
-- "reverseOrder": true} HTTP/1.1 200 OK Content-Length: 174 Content-Type:
-- application/json x-amzn-RequestId: dcde6719-3fc7-11e1-9e8f-57bb03e21482
-- {"typeInfos": [ {"creationDate": 1326481174.027, "description": "Handle
-- customer orders", "status": "REGISTERED", "workflowType": {"name":
-- "customerOrderWorkflow", "version": "1.0"} } ] }.
--
-- See: 'Network.AWS.SWF.V2012_01_25.ListWorkflowTypes'
listWorkflowTypes :: ( MonadCatch m
                     , MonadResource m
                     , MonadError Error m
                     , MonadReader Env (ResumableSource m)
                     , AWSPager a
                     )
                  => Text -- ^ 'lwtDomain'
                  -> RegistrationStatus -- ^ 'lwtRegistrationStatus'
                  -> State ListWorkflowTypes a
                  -> ResumableSource m ListWorkflowTypesResponse
listWorkflowTypes p1 p3 s =
    paginate $ (mkListWorkflowTypes p1 p3) &~ s

-- | Used by workers to get an ActivityTask from the specified activity
-- taskList. This initiates a long poll, where the service holds the HTTP
-- connection open and responds as soon as a task becomes available. The
-- maximum time the service holds on to the request before responding is 60
-- seconds. If no task is available within 60 seconds, the poll will return an
-- empty result. An empty result, in this context, means that an ActivityTask
-- is returned, but that the value of taskToken is an empty string. If a task
-- is returned, the worker should use its type to identify and process it
-- correctly. Workers should set their client side socket timeout to at least
-- 70 seconds (10 seconds higher than the maximum time service may hold the
-- poll request). Access Control You can use IAM policies to control this
-- action's access to Amazon SWF resources as follows: Use a Resource element
-- with the domain name to limit the action to only specified domains. Use an
-- Action element to allow or deny permission to call this action. Constrain
-- the taskList.name parameter by using a Condition element with the
-- swf:taskList.name key to allow the action to access only certain task
-- lists. If the caller does not have sufficient permissions to invoke the
-- action, or the parameter values fall outside the specified constraints, the
-- action fails by throwing OperationNotPermitted. For details and example IAM
-- policies, see Using IAM to Manage Access to Amazon SWF Workflows.
-- PollForActivityTask Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 03:53:52 GMT X-Amz-Target:
-- SimpleWorkflowService.PollForActivityTask Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=dv0H1RPYucoIcRckspWO0f8xG120MWZRKmj3O5/A4rY=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 108 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "taskList": {"name": "mainTaskList"}, "identity":
-- "VerifyCreditCardWorker01"} HTTP/1.1 200 OK Content-Length: 837
-- Content-Type: application/json x-amzn-RequestId:
-- b48fb6b5-3ff5-11e1-a23a-99d60383ae71 {"activityId": "verification-27",
-- "activityType": {"name": "activityVerify", "version": "1.0"}, "input":
-- "5634-0056-4367-0923,12/12,437", "startedEventId": 11, "taskToken":
-- "AAAAKgAAAAEAAAAAAAAAAX9p3pcp3857oLXFUuwdxRU5/zmn9f40XaMF7VohAH4jOtjXpZu7GdOzEi0b3cWYHbG5b5dpdcTXHUDPVMHXiUxCgr+Nc/wUW9016W4YxJGs/jmxzPln8qLftU+SW135Q0UuKp5XRGoRTJp3tbHn2pY1vC8gDB/K69J6q668U1pd4Cd9o43//lGgOIjN0/Ihg+DO+83HNcOuVEQMM28kNMXf7yePh31M4dMKJwQaQZG13huJXDwzJOoZQz+XFuqFly+lPnCE4XvsnhfAvTsh50EtNDEtQzPCFJoUeld9g64V/FS/39PHL3M93PBUuroPyHuCwHsNC6fZ7gM/XOKmW4kKnXPoQweEUkFV/J6E6+M1reBO7nJADTrLSnajg6MY/viWsEYmMw/DS5FlquFaDIhFkLhWUWN+V2KqiKS23GYwpzgZ7fgcWHQF2NLEY3zrjam4LW/UW5VLCyM3FpVD3erCTi9IvUgslPzyVGuWNAoTmgJEWvimgwiHxJMxxc9JBDR390iMmImxVl3eeSDUWx8reQltiviadPDjyRmVhYP8",
-- "workflowExecution": {"runId": "cfa2bd33-31b0-4b75-b131-255bb0d97b3f",
-- "workflowId": "20110927-T-1"} }.
--
-- See: 'Network.AWS.SWF.V2012_01_25.PollForActivityTask'
pollForActivityTask :: ( MonadCatch m
                       , MonadResource m
                       , MonadError Error m
                       , MonadReader Env m
                       , AWSRequest a
                       )
                    => Text -- ^ 'pfatDomain'
                    -> TaskList -- ^ 'pfatTaskList'
                    -> State PollForActivityTask a
                    -> m PollForActivityTaskResponse
pollForActivityTask p1 p2 s =
    send $ (mkPollForActivityTask p1 p2) &~ s

-- | Used by deciders to get a DecisionTask from the specified decision
-- taskList. A decision task may be returned for any open workflow execution
-- that is using the specified task list. The task includes a paginated view
-- of the history of the workflow execution. The decider should use the
-- workflow type and the history to determine how to properly handle the task.
-- This action initiates a long poll, where the service holds the HTTP
-- connection open and responds as soon a task becomes available. If no
-- decision task is available in the specified task list before the timeout of
-- 60 seconds expires, an empty result is returned. An empty result, in this
-- context, means that a DecisionTask is returned, but that the value of
-- taskToken is an empty string. Deciders should set their client side socket
-- timeout to at least 70 seconds (10 seconds higher than the timeout).
-- Because the number of workflow history events for a single workflow
-- execution might be very large, the result returned might be split up across
-- a number of pages. To retrieve subsequent pages, make additional calls to
-- PollForDecisionTask using the nextPageToken returned by the initial call.
-- Note that you do not call GetWorkflowExecutionHistory with this
-- nextPageToken. Instead, call PollForDecisionTask again. Access Control You
-- can use IAM policies to control this action's access to Amazon SWF
-- resources as follows: Use a Resource element with the domain name to limit
-- the action to only specified domains. Use an Action element to allow or
-- deny permission to call this action. Constrain the taskList.name parameter
-- by using a Condition element with the swf:taskList.name key to allow the
-- action to access only certain task lists. If the caller does not have
-- sufficient permissions to invoke the action, or the parameter values fall
-- outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. PollForDecisionTask Example POST
-- / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 02:09:54 GMT
-- X-Amz-Target: SimpleWorkflowService.PollForDecisionTask Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=R3CJ2HMLSVpc2p6eafeztZCZWcgza+h61gSUuWx15gw=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 171 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "taskList": {"name": "specialTaskList"}, "identity":
-- "Decider01", "maximumPageSize": 50, "reverseOrder": true} HTTP/1.1 200 OK
-- Content-Length: 1639 Content-Type: application/json x-amzn-RequestId:
-- 03db54cf-3f1e-11e1-b118-3bfa5e8e7fc3 {"events": [
-- {"decisionTaskStartedEventAttributes": {"identity": "Decider01",
-- "scheduledEventId": 2}, "eventId": 3, "eventTimestamp": 1326593394.566,
-- "eventType": "DecisionTaskStarted"},
-- {"decisionTaskScheduledEventAttributes": {"startToCloseTimeout": "600",
-- "taskList": {"name": "specialTaskList"} }, "eventId": 2, "eventTimestamp":
-- 1326592619.474, "eventType": "DecisionTaskScheduled"}, {"eventId": 1,
-- "eventTimestamp": 1326592619.474, "eventType": "WorkflowExecutionStarted",
-- "workflowExecutionStartedEventAttributes": {"childPolicy": "TERMINATE",
-- "executionStartToCloseTimeout": "3600", "input":
-- "arbitrary-string-that-is-meaningful-to-the-workflow",
-- "parentInitiatedEventId": 0, "tagList": ["music purchase", "digital",
-- "ricoh-the-dog"], "taskList": {"name": "specialTaskList"},
-- "taskStartToCloseTimeout": "600", "workflowType": {"name":
-- "customerOrderWorkflow", "version": "1.0"} } } ], "previousStartedEventId":
-- 0, "startedEventId": 3, "taskToken":
-- "AAAAKgAAAAEAAAAAAAAAATZDvCYwk/hP/X1ZGdJhb+T6OWzcBx2DPhsIi5HF4aGQI4OXrDE7Ny3uM+aiAhGrmeNyVAa4yNIBQuoZuJA5G+BoaB0JuHFBOynHDTnm7ayNH43KhMkfdrDG4elfHSz3m/EtbLnFGueAr7+3NKDG6x4sTKg3cZpOtSguSx05yI1X3AtscS8ATcLB2Y3Aub1YonN/i/k67voca/GFsSiwSz3AAnJj1IPvrujgIj9KUvckwRPC5ET7d33XJcRp+gHYzZsBLVBaRmV3gEYAnp2ICslFn4YSjGy+dFXCNpOa4G1O8pczCbFUGbQ3+5wf0RSaa/xMq2pfdBKnuFp0wp8kw1k+5ZsbtDZeZn8g5GyKCLiLms/xD0OxugGGUe5ZlAoHEkTWGxZj/G32P7cMoCgrcACfFPdx1LNYYEre7YiGiyjGnfW2t5mW7VK9Np28vcXVbdpH4JNEB9OuB1xqL8N8ifPVtc72uxB1i9XEdq/8rkXasSEw4TubB2FwgqnuJstmfEhpOdb5HfhR6OwmnHuk9eszO/fUkGucTUXQP2hhB+Gz",
-- "workflowExecution": {"runId": "06b8f87a-24b3-40b6-9ceb-9676f28e9493",
-- "workflowId": "20110927-T-1"}, "workflowType": {"name":
-- "customerOrderWorkflow", "version": "1.0"} }.
--
-- See: 'Network.AWS.SWF.V2012_01_25.PollForDecisionTask'
pollForDecisionTask :: ( MonadCatch m
                       , MonadResource m
                       , MonadError Error m
                       , MonadReader Env (ResumableSource m)
                       , AWSPager a
                       )
                    => Text -- ^ 'pfdtDomain'
                    -> TaskList -- ^ 'pfdtTaskList'
                    -> State PollForDecisionTask a
                    -> ResumableSource m PollForDecisionTaskResponse
pollForDecisionTask p1 p2 s =
    paginate $ (mkPollForDecisionTask p1 p2) &~ s

-- | Used by activity workers to report to the service that the ActivityTask
-- represented by the specified taskToken is still making progress. The worker
-- can also (optionally) specify details of the progress, for example percent
-- complete, using the details parameter. This action can also be used by the
-- worker as a mechanism to check if cancellation is being requested for the
-- activity task. If a cancellation is being attempted for the specified task,
-- then the boolean cancelRequested flag returned by the service is set to
-- true. This action resets the taskHeartbeatTimeout clock. The
-- taskHeartbeatTimeout is specified in RegisterActivityType. This action does
-- not in itself create an event in the workflow execution history. However,
-- if the task times out, the workflow execution history will contain a
-- ActivityTaskTimedOut event that contains the information from the last
-- heartbeat generated by the activity worker. The taskStartToCloseTimeout of
-- an activity type is the maximum duration of an activity task, regardless of
-- the number of RecordActivityTaskHeartbeat requests received. The
-- taskStartToCloseTimeout is also specified in RegisterActivityType. This
-- operation is only useful for long-lived activities to report liveliness of
-- the task and to determine if a cancellation is being attempted. If the
-- cancelRequested flag returns true, a cancellation is being attempted. If
-- the worker can cancel the activity, it should respond with
-- RespondActivityTaskCanceled. Otherwise, it should ignore the cancellation
-- request. Access Control You can use IAM policies to control this action's
-- access to Amazon SWF resources as follows: Use a Resource element with the
-- domain name to limit the action to only specified domains. Use an Action
-- element to allow or deny permission to call this action. You cannot use an
-- IAM policy to constrain this action's parameters. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. RecordActivityTaskHeartbeat
-- Example POST / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent:
-- Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 03:55:06 GMT
-- X-Amz-Target: SimpleWorkflowService.RecordActivityTaskHeartbeat
-- Content-Encoding: amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=DEA8rw5TqtpqCeTljl7eotZkuWTgmGZ1PWyDNZPehT0=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 623 Pragma: no-cache Cache-Control: no-cache {"taskToken":
-- "AAAAKgAAAAEAAAAAAAAAAX9p3pcp3857oLXFUuwdxRU5/zmn9f40XaMF7VohAH4jOtjXpZu7GdOzEi0b3cWYHbG5b5dpdcTXHUDPVMHXiUxCgr+Nc/wUW9016W4YxJGs/jmxzPln8qLftU+SW135Q0UuKp5XRGoRTJp3tbHn2pY1vC8gDB/K69J6q668U1pd4Cd9o43//lGgOIjN0/Ihg+DO+83HNcOuVEQMM28kNMXf7yePh31M4dMKJwQaQZG13huJXDwzJOoZQz+XFuqFly+lPnCE4XvsnhfAvTsh50EtNDEtQzPCFJoUeld9g64V/FS/39PHL3M93PBUuroPyHuCwHsNC6fZ7gM/XOKmW4kKnXPoQweEUkFV/J6E6+M1reBO7nJADTrLSnajg6MY/viWsEYmMw/DS5FlquFaDIhFkLhWUWN+V2KqiKS23GYwpzgZ7fgcWHQF2NLEY3zrjam4LW/UW5VLCyM3FpVD3erCTi9IvUgslPzyVGuWNAoTmgJEWvimgwiHxJMxxc9JBDR390iMmImxVl3eeSDUWx8reQltiviadPDjyRmVhYP8",
-- "details": "starting task"} HTTP/1.1 200 OK Content-Length: 25
-- Content-Type: application/json x-amzn-RequestId:
-- e08622cd-3ff5-11e1-9b11-7182192d0b57 {"cancelRequested":false}.
--
-- See: 'Network.AWS.SWF.V2012_01_25.RecordActivityTaskHeartbeat'
recordActivityTaskHeartbeat :: ( MonadCatch m
                               , MonadResource m
                               , MonadError Error m
                               , MonadReader Env m
                               , AWSRequest a
                               )
                            => Text -- ^ 'rathTaskToken'
                            -> State RecordActivityTaskHeartbeat a
                            -> m RecordActivityTaskHeartbeatResponse
recordActivityTaskHeartbeat p1 s =
    send $ (mkRecordActivityTaskHeartbeat p1) &~ s

-- | Registers a new activity type along with its configuration settings in the
-- specified domain. A TypeAlreadyExists fault is returned if the type already
-- exists in the domain. You cannot change any configuration settings of the
-- type after its registration, and it must be registered as a new version.
-- Access Control You can use IAM policies to control this action's access to
-- Amazon SWF resources as follows: Use a Resource element with the domain
-- name to limit the action to only specified domains. Use an Action element
-- to allow or deny permission to call this action. Constrain the following
-- parameters by using a Condition element with the appropriate keys.
-- defaultTaskList.name: String constraint. The key is
-- swf:defaultTaskList.name. name: String constraint. The key is swf:name.
-- version: String constraint. The key is swf:version. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. RegisterActivityType Example POST
-- / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 00:14:06 GMT
-- X-Amz-Target: SimpleWorkflowService.RegisterActivityType Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=F9cptqaGWa2H7LW3dpctF9J5svsB6FRZ4krghCRnml0=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 343 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "name": "activityVerify", "version": "1.0", "description":
-- "Verify the customer credit card", "defaultTaskStartToCloseTimeout": "600",
-- "defaultTaskHeartbeatTimeout": "120", "defaultTaskList": {"name":
-- "mainTaskList"}, "defaultTaskScheduleToStartTimeout": "300",
-- "defaultTaskScheduleToCloseTimeout": "900"} HTTP/1.1 200 OK Content-Length:
-- 0 Content-Type: application/json x-amzn-RequestId:
-- d68969c7-3f0d-11e1-9b11-7182192d0b57.
--
-- See: 'Network.AWS.SWF.V2012_01_25.RegisterActivityType'
registerActivityType :: ( MonadCatch m
                        , MonadResource m
                        , MonadError Error m
                        , MonadReader Env m
                        , AWSRequest a
                        )
                     => Text -- ^ 'ratDomain'
                     -> Text -- ^ 'ratName'
                     -> Text -- ^ 'ratVersion'
                     -> State RegisterActivityType a
                     -> m RegisterActivityTypeResponse
registerActivityType p1 p2 p3 s =
    send $ (mkRegisterActivityType p1 p2 p3) &~ s

-- | Registers a new domain. Access Control You can use IAM policies to control
-- this action's access to Amazon SWF resources as follows: You cannot use an
-- IAM policy to control domain access for this action. The name of the domain
-- being registered is available as the resource of this action. Use an Action
-- element to allow or deny permission to call this action. You cannot use an
-- IAM policy to constrain this action's parameters. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. RegisterDomain Example POST /
-- HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Fri, 13 Jan 2012 18:42:12 GMT
-- X-Amz-Target: SimpleWorkflowService.RegisterDomain Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=tzjkF55lxAxPhzp/BRGFYQRQRq6CqrM254dTDE/EncI=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 91 Pragma: no-cache Cache-Control: no-cache {"name":
-- "867530902", "description": "music",
-- "workflowExecutionRetentionPeriodInDays": "60"} HTTP/1.1 200 OK
-- Content-Length: 0 Content-Type: application/json x-amzn-RequestId:
-- 4ec4ac3f-3e16-11e1-9b11-7182192d0b57.
--
-- See: 'Network.AWS.SWF.V2012_01_25.RegisterDomain'
registerDomain :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env m
                  , AWSRequest a
                  )
               => Text -- ^ 'rdName'
               -> Text -- ^ 'rdWorkflowExecutionRetentionPeriodInDays'
               -> State RegisterDomain a
               -> m RegisterDomainResponse
registerDomain p1 p3 s =
    send $ (mkRegisterDomain p1 p3) &~ s

-- | Registers a new workflow type and its configuration settings in the
-- specified domain. The retention period for the workflow history is set by
-- the RegisterDomain action. If the type already exists, then a
-- TypeAlreadyExists fault is returned. You cannot change the configuration
-- settings of a workflow type once it is registered and it must be registered
-- as a new version. Access Control You can use IAM policies to control this
-- action's access to Amazon SWF resources as follows: Use a Resource element
-- with the domain name to limit the action to only specified domains. Use an
-- Action element to allow or deny permission to call this action. Constrain
-- the following parameters by using a Condition element with the appropriate
-- keys. defaultTaskList.name: String constraint. The key is
-- swf:defaultTaskList.name. name: String constraint. The key is swf:name.
-- version: String constraint. The key is swf:version. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. RegisterWorkflowType Example POST
-- / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Fri, 13 Jan 2012 18:59:33 GMT
-- X-Amz-Target: SimpleWorkflowService.RegisterWorkflowType Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=p5FUOoV3QXAafb7aK5z79Ztu5v0w9NeEqLu0ei+P9FA=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 300 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "name": "customerOrderWorkflow", "version": "1.0",
-- "description": "Handle customer orders", "defaultTaskStartToCloseTimeout":
-- "600", "defaultExecutionStartToCloseTimeout": "3600", "defaultTaskList":
-- {"name": "mainTaskList"}, "defaultChildPolicy": "TERMINATE"} HTTP/1.1 200
-- OK Content-Length: 0 Content-Type: application/json x-amzn-RequestId:
-- bb469e67-3e18-11e1-9914-a356b6ea8bdf.
--
-- See: 'Network.AWS.SWF.V2012_01_25.RegisterWorkflowType'
registerWorkflowType :: ( MonadCatch m
                        , MonadResource m
                        , MonadError Error m
                        , MonadReader Env m
                        , AWSRequest a
                        )
                     => Text -- ^ 'rwtDomain'
                     -> Text -- ^ 'rwtName'
                     -> Text -- ^ 'rwtVersion'
                     -> State RegisterWorkflowType a
                     -> m RegisterWorkflowTypeResponse
registerWorkflowType p1 p2 p3 s =
    send $ (mkRegisterWorkflowType p1 p2 p3) &~ s

-- | Records a WorkflowExecutionCancelRequested event in the currently running
-- workflow execution identified by the given domain, workflowId, and runId.
-- This logically requests the cancellation of the workflow execution as a
-- whole. It is up to the decider to take appropriate actions when it receives
-- an execution history with this event. If the runId is not specified, the
-- WorkflowExecutionCancelRequested event is recorded in the history of the
-- current open workflow execution with the specified workflowId in the
-- domain. Because this action allows the workflow to properly clean up and
-- gracefully close, it should be used instead of TerminateWorkflowExecution
-- when possible. Access Control You can use IAM policies to control this
-- action's access to Amazon SWF resources as follows: Use a Resource element
-- with the domain name to limit the action to only specified domains. Use an
-- Action element to allow or deny permission to call this action. You cannot
-- use an IAM policy to constrain this action's parameters. If the caller does
-- not have sufficient permissions to invoke the action, or the parameter
-- values fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. RequestCancelWorkflowExecution
-- Example POST / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent:
-- Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 04:49:06 GMT
-- X-Amz-Target: SimpleWorkflowService.RequestCancelWorkflowExecution
-- Content-Encoding: amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=xODwV3kbpJbWVa6bQiV2zQAw9euGI3uXI82urc+bVeo=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 106 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "workflowId": "20110927-T-1", "runId":
-- "94861fda-a714-4126-95d7-55ba847da8ab"} HTTP/1.1 200 OK Content-Length: 0
-- Content-Type: application/json x-amzn-RequestId:
-- 6bd0627e-3ffd-11e1-9b11-7182192d0b57.
--
-- See: 'Network.AWS.SWF.V2012_01_25.RequestCancelWorkflowExecution'
requestCancelWorkflowExecution :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadError Error m
                                  , MonadReader Env m
                                  , AWSRequest a
                                  )
                               => Text -- ^ 'rcweDomain'
                               -> Text -- ^ 'rcweWorkflowId'
                               -> State RequestCancelWorkflowExecution a
                               -> m RequestCancelWorkflowExecutionResponse
requestCancelWorkflowExecution p1 p2 s =
    send $ (mkRequestCancelWorkflowExecution p1 p2) &~ s

-- | Used by workers to tell the service that the ActivityTask identified by the
-- taskToken was successfully canceled. Additional details can be optionally
-- provided using the details argument. These details (if provided) appear in
-- the ActivityTaskCanceled event added to the workflow history. Only use this
-- operation if the canceled flag of a RecordActivityTaskHeartbeat request
-- returns true and if the activity can be safely undone or abandoned. A task
-- is considered open from the time that it is scheduled until it is closed.
-- Therefore a task is reported as open while a worker is processing it. A
-- task is closed after it has been specified in a call to
-- RespondActivityTaskCompleted, RespondActivityTaskCanceled,
-- RespondActivityTaskFailed, or the task has timed out. Access Control You
-- can use IAM policies to control this action's access to Amazon SWF
-- resources as follows: Use a Resource element with the domain name to limit
-- the action to only specified domains. Use an Action element to allow or
-- deny permission to call this action. You cannot use an IAM policy to
-- constrain this action's parameters. If the caller does not have sufficient
-- permissions to invoke the action, or the parameter values fall outside the
-- specified constraints, the action fails by throwing OperationNotPermitted.
-- For details and example IAM policies, see Using IAM to Manage Access to
-- Amazon SWF Workflows. RespondActivityTaskCanceled Example POST / HTTP/1.1
-- Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U;
-- Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET
-- CLR 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 04:36:44 GMT X-Amz-Target:
-- SimpleWorkflowService.RespondActivityTaskCanceled Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=7ZMb0Np0OyXw6hrFSBFDAfBnSaEP1TH7cAG29DL5BUI=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 640 Pragma: no-cache Cache-Control: no-cache {"taskToken":
-- "AAAAKgAAAAEAAAAAAAAAAQlFok8Ay875ki85gos/Okm9kWg1Jm6DbwiBZgxyCrW2OS+DQQtrCTMr+KH1ouxrCVOkTXPOUY/M4Ujfr1CrsMi6S0DMD8/N6yxzd34+PIIvRY8w9M5z89PbPQKjKHKbz2ocbTnHgRThaBO4ZmeadNyZWSeQyZXmsQFmFuHfaH9P2ibzrDS1dU+s/iw/R9RBrRWArsph/FIfWdRUJfu/FH9IFPSb3KYKMVaJAOyWhcR1KrRGywIGxPC7m9tQjapXqitoRYj42qgABydT4NVR5cLCkeYW0LKxUGVU46+gNvRaUfYzP31JVARQh5d0j7S/ERi10m6bamPJ3UcZfLFbM42mIINywmcTORMpQ/nPGLU1iECYrtnAV0YTlGZfGm+Vi6Gcgwyi4hEjg7TCBjc6WBw3JuAfFvUPU5cfvAoX7quUZRA7JUnYGObE0y9zYuTnCx6C1GL7Ks2MEA0coIiAl4JZx6qsGYfeKjIGntTsoCEe1zjp5gRqfeD74kfeZg0HmqA0xiFGZ40OHbImnF5YHsedYfLk6u09SAkQMD8iJhT8",
-- "details": "customer canceled transaction"} HTTP/1.1 200 OK Content-Length:
-- 0 Content-Type: application/json x-amzn-RequestId:
-- b1a001a6-3ffb-11e1-9b11-7182192d0b57.
--
-- See: 'Network.AWS.SWF.V2012_01_25.RespondActivityTaskCanceled'
respondActivityTaskCanceled :: ( MonadCatch m
                               , MonadResource m
                               , MonadError Error m
                               , MonadReader Env m
                               , AWSRequest a
                               )
                            => Text -- ^ 'ratcTaskToken'
                            -> State RespondActivityTaskCanceled a
                            -> m RespondActivityTaskCanceledResponse
respondActivityTaskCanceled p1 s =
    send $ (mkRespondActivityTaskCanceled p1) &~ s

-- | Used by workers to tell the service that the ActivityTask identified by the
-- taskToken completed successfully with a result (if provided). The result
-- appears in the ActivityTaskCompleted event in the workflow history. If the
-- requested task does not complete successfully, use
-- RespondActivityTaskFailed instead. If the worker finds that the task is
-- canceled through the canceled flag returned by RecordActivityTaskHeartbeat,
-- it should cancel the task, clean up and then call
-- RespondActivityTaskCanceled. A task is considered open from the time that
-- it is scheduled until it is closed. Therefore a task is reported as open
-- while a worker is processing it. A task is closed after it has been
-- specified in a call to RespondActivityTaskCompleted,
-- RespondActivityTaskCanceled, RespondActivityTaskFailed, or the task has
-- timed out. Access Control You can use IAM policies to control this action's
-- access to Amazon SWF resources as follows: Use a Resource element with the
-- domain name to limit the action to only specified domains. Use an Action
-- element to allow or deny permission to call this action. You cannot use an
-- IAM policy to constrain this action's parameters. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. RespondActivityTaskCompleted
-- Example POST / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent:
-- Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 03:56:15 GMT
-- X-Amz-Target: SimpleWorkflowService.RespondActivityTaskCompleted
-- Content-Encoding: amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=M+ygHbMHSHJiVrsAQTW/BfkgHoNzLPnPD+dVywJiPXE=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 638 Pragma: no-cache Cache-Control: no-cache {"taskToken":
-- "AAAAKgAAAAEAAAAAAAAAAX9p3pcp3857oLXFUuwdxRU5/zmn9f40XaMF7VohAH4jOtjXpZu7GdOzEi0b3cWYHbG5b5dpdcTXHUDPVMHXiUxCgr+Nc/wUW9016W4YxJGs/jmxzPln8qLftU+SW135Q0UuKp5XRGoRTJp3tbHn2pY1vC8gDB/K69J6q668U1pd4Cd9o43//lGgOIjN0/Ihg+DO+83HNcOuVEQMM28kNMXf7yePh31M4dMKJwQaQZG13huJXDwzJOoZQz+XFuqFly+lPnCE4XvsnhfAvTsh50EtNDEtQzPCFJoUeld9g64V/FS/39PHL3M93PBUuroPyHuCwHsNC6fZ7gM/XOKmW4kKnXPoQweEUkFV/J6E6+M1reBO7nJADTrLSnajg6MY/viWsEYmMw/DS5FlquFaDIhFkLhWUWN+V2KqiKS23GYwpzgZ7fgcWHQF2NLEY3zrjam4LW/UW5VLCyM3FpVD3erCTi9IvUgslPzyVGuWNAoTmgJEWvimgwiHxJMxxc9JBDR390iMmImxVl3eeSDUWx8reQltiviadPDjyRmVhYP8",
-- "result": "customer credit card verified"} HTTP/1.1 200 OK Content-Length:
-- 0 Content-Type: application/json x-amzn-RequestId:
-- 0976f0f4-3ff6-11e1-9a27-0760db01a4a8.
--
-- See: 'Network.AWS.SWF.V2012_01_25.RespondActivityTaskCompleted'
respondActivityTaskCompleted :: ( MonadCatch m
                                , MonadResource m
                                , MonadError Error m
                                , MonadReader Env m
                                , AWSRequest a
                                )
                             => Text -- ^ 'ratc1TaskToken'
                             -> State RespondActivityTaskCompleted a
                             -> m RespondActivityTaskCompletedResponse
respondActivityTaskCompleted p1 s =
    send $ (mkRespondActivityTaskCompleted p1) &~ s

-- | Used by workers to tell the service that the ActivityTask identified by the
-- taskToken has failed with reason (if specified). The reason and details
-- appear in the ActivityTaskFailed event added to the workflow history. A
-- task is considered open from the time that it is scheduled until it is
-- closed. Therefore a task is reported as open while a worker is processing
-- it. A task is closed after it has been specified in a call to
-- RespondActivityTaskCompleted, RespondActivityTaskCanceled,
-- RespondActivityTaskFailed, or the task has timed out. Access Control You
-- can use IAM policies to control this action's access to Amazon SWF
-- resources as follows: Use a Resource element with the domain name to limit
-- the action to only specified domains. Use an Action element to allow or
-- deny permission to call this action. You cannot use an IAM policy to
-- constrain this action's parameters. If the caller does not have sufficient
-- permissions to invoke the action, or the parameter values fall outside the
-- specified constraints, the action fails by throwing OperationNotPermitted.
-- For details and example IAM policies, see Using IAM to Manage Access to
-- Amazon SWF Workflows. RespondActivityTaskFailed Example POST / HTTP/1.1
-- Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U;
-- Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET
-- CLR 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 04:17:24 GMT X-Amz-Target:
-- SimpleWorkflowService.RespondActivityTaskFailed Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=JC+/uds/mFEq8qca2WFs5kfp2eAEONc70IqFgHErhpc=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 682 Pragma: no-cache Cache-Control: no-cache {"taskToken":
-- "AAAAKgAAAAEAAAAAAAAAAdG7j7YFEl9pfKdXRL3Cy3Q3c1Z8QwdOSX53bKiUV6MMGXvf3Lrinmmzj1HFFl5lcwHzEFxLbMaSZ/lMt/RFJPumHXAnUqlYjZLODhrBqsIzDQFKcbCFMq7y4jm0EFzsV2Suv8iu/obcZ/idU8qjd9uG/82zumG2xz1Z4IbOFwOTlpj2++5YVH4ftyycIcjlDw58r0O1vAo4PEondkqjyn+YxBxyZLy1z1fvMi0zeO8Lh16w96y6v+KdVc/ECoez1Og8sROaXG0l8ptW5YR733LIuUBK4sxWa12egF5i4e8AV8JloojOaq0jy4iFsIscRazOSQErjo15Guz89BK2XW911P3I+X7nJjH0wwW55XGCs0jezvsEC8M6D9Ob7CgWr6RrnK3g1AKemcby2XqgQRN52DMIYxzV+lMS/QBYKOqtkLoMY0NKeuRVwm9f1zCY00v6kxqK9m2zFvaxqlJ5/JVCWMNWEWJfQZVtC3GzMWmzeCt7Auq8A5/Caq/DKyOhTIhY/Go00iiDA6ecP8taTYiVzb8VR5xEiQ1uCxnECkwW",
-- "reason": "could not verify customer credit card", "details": "card number
-- invalid"} HTTP/1.1 200 OK Content-Length: 0 Content-Type: application/json
-- x-amzn-RequestId: feadaedd-3ff8-11e1-9e8f-57bb03e21482.
--
-- See: 'Network.AWS.SWF.V2012_01_25.RespondActivityTaskFailed'
respondActivityTaskFailed :: ( MonadCatch m
                             , MonadResource m
                             , MonadError Error m
                             , MonadReader Env m
                             , AWSRequest a
                             )
                          => Text -- ^ 'ratfTaskToken'
                          -> State RespondActivityTaskFailed a
                          -> m RespondActivityTaskFailedResponse
respondActivityTaskFailed p1 s =
    send $ (mkRespondActivityTaskFailed p1) &~ s

-- | Used by deciders to tell the service that the DecisionTask identified by
-- the taskToken has successfully completed. The decisions argument specifies
-- the list of decisions made while processing the task. A
-- DecisionTaskCompleted event is added to the workflow history. The
-- executionContext specified is attached to the event in the workflow
-- execution history. Access Control If an IAM policy grants permission to use
-- RespondDecisionTaskCompleted, it can express permissions for the list of
-- decisions in the decisions parameter. Each of the decisions has one or more
-- parameters, much like a regular API call. To allow for policies to be as
-- readable as possible, you can express permissions on decisions as if they
-- were actual API calls, including applying conditions to some parameters.
-- For more information, see Using IAM to Manage Access to Amazon SWF
-- Workflows. RespondDecisionTaskCompleted Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 23:31:06 GMT X-Amz-Target:
-- SimpleWorkflowService.RespondDecisionTaskCompleted Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=FL4ouCb8n6j5egcKOXoa+5Vctc8WmA91B2ekKnks2J8=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 1184 Pragma: no-cache Cache-Control: no-cache {"taskToken":
-- "AAAAKgAAAAEAAAAAAAAAAQLPoqDSLcx4ksNCEQZCyEBqpKhE+FgFSOvHd9zlCROacKYHh640MkANx2y9YM3CQnec0kEb1oRvB6DxKesTY3U/UQhvBqPY7E4BYE6hkDj/NmSbt9EwEJ/a+WD+oc2sDNfeVz2x+6wjb5vQdFKwBoQ6MDWLFbAhcgK+ymoRjoBHrPsrNLX3IA6sQaPmQRZQs3FRZonoVzP6uXMCZPnCZQULFjU1kTM8VHzH7ywqWKVmmdvnqyREOCT9VqmYbhLntJXsDj+scAvuNy17MCX9M9AJ7V/5qrLCeYdWA4FBQgY4Ew6IC+dge/UZdVMmpW/uB7nvSk6owQIhapPh5pEUwwY/yNnoVLTiPOz9KzZlANyw7uDchBRLvUJORFtpP9ZQIouNP8QOvFWm7Idc50ahwGEdTCiG+KDXV8kAzx7wKHs7l1TXYkC15x0h3XPH0MdLeEjipv98EpZaMIVtgGSdRjluOjNWEL2zowZByitleI5bdvxZdgalAXXKEnbYE6/rfLGReAJKdh2n0dmTMI+tK7uuxIWX6F4ocqSI1Xb2x5zZ",
-- "decisions": [ {"decisionType": "ScheduleActivityTask",
-- "scheduleActivityTaskDecisionAttributes": {"activityType": {"name":
-- "activityVerify", "version": "1.0"}, "activityId": "verification-27",
-- "control": "digital music", "input": "5634-0056-4367-0923,12/12,437",
-- "scheduleToCloseTimeout": "900", "taskList": {"name": "specialTaskList"},
-- "scheduleToStartTimeout": "300", "startToCloseTimeout": "600",
-- "heartbeatTimeout": "120"} } ], "executionContext": "Black Friday"}
-- HTTP/1.1 200 OK Content-Length: 0 Content-Type: application/json
-- x-amzn-RequestId: feef79b5-3fd0-11e1-9a27-0760db01a4a8.
--
-- See: 'Network.AWS.SWF.V2012_01_25.RespondDecisionTaskCompleted'
respondDecisionTaskCompleted :: ( MonadCatch m
                                , MonadResource m
                                , MonadError Error m
                                , MonadReader Env m
                                , AWSRequest a
                                )
                             => Text -- ^ 'rdtcTaskToken'
                             -> State RespondDecisionTaskCompleted a
                             -> m RespondDecisionTaskCompletedResponse
respondDecisionTaskCompleted p1 s =
    send $ (mkRespondDecisionTaskCompleted p1) &~ s

-- | Records a WorkflowExecutionSignaled event in the workflow execution history
-- and creates a decision task for the workflow execution identified by the
-- given domain, workflowId and runId. The event is recorded with the
-- specified user defined signalName and input (if provided). If a runId is
-- not specified, then the WorkflowExecutionSignaled event is recorded in the
-- history of the current open workflow with the matching workflowId in the
-- domain. If the specified workflow execution is not open, this method fails
-- with UnknownResource. Access Control You can use IAM policies to control
-- this action's access to Amazon SWF resources as follows: Use a Resource
-- element with the domain name to limit the action to only specified domains.
-- Use an Action element to allow or deny permission to call this action. You
-- cannot use an IAM policy to constrain this action's parameters. If the
-- caller does not have sufficient permissions to invoke the action, or the
-- parameter values fall outside the specified constraints, the action fails
-- by throwing OperationNotPermitted. For details and example IAM policies,
-- see Using IAM to Manage Access to Amazon SWF Workflows.
-- SignalWorkflowExecution Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 00:06:18 GMT X-Amz-Target:
-- SimpleWorkflowService.SignalWorkflowExecution Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=lQpBZezK7JNQrXeWuJE+l7S0ZwjOEONCeRyImoyfX+E=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 162 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "workflowId": "20110927-T-1", "runId":
-- "f5ebbac6-941c-4342-ad69-dfd2f8be6689", "signalName": "CancelOrder",
-- "input": "order 3553"} HTTP/1.1 200 OK Content-Length: 0 Content-Type:
-- application/json x-amzn-RequestId: bf78ae15-3f0c-11e1-9914-a356b6ea8bdf.
--
-- See: 'Network.AWS.SWF.V2012_01_25.SignalWorkflowExecution'
signalWorkflowExecution :: ( MonadCatch m
                           , MonadResource m
                           , MonadError Error m
                           , MonadReader Env m
                           , AWSRequest a
                           )
                        => Text -- ^ 'sweDomain'
                        -> Text -- ^ 'sweWorkflowId'
                        -> Text -- ^ 'sweSignalName'
                        -> State SignalWorkflowExecution a
                        -> m SignalWorkflowExecutionResponse
signalWorkflowExecution p1 p2 p4 s =
    send $ (mkSignalWorkflowExecution p1 p2 p4) &~ s

-- | Starts an execution of the workflow type in the specified domain using the
-- provided workflowId and input data. This action returns the newly started
-- workflow execution. Access Control You can use IAM policies to control this
-- action's access to Amazon SWF resources as follows: Use a Resource element
-- with the domain name to limit the action to only specified domains. Use an
-- Action element to allow or deny permission to call this action. Constrain
-- the following parameters by using a Condition element with the appropriate
-- keys. tagList.member.0: The key is swf:tagList.member.0. tagList.member.1:
-- The key is swf:tagList.member.1. tagList.member.2: The key is
-- swf:tagList.member.2. tagList.member.3: The key is swf:tagList.member.3.
-- tagList.member.4: The key is swf:tagList.member.4. taskList: String
-- constraint. The key is swf:taskList.name. name: String constraint. The key
-- is swf:workflowType.name. version: String constraint. The key is
-- swf:workflowType.version. If the caller does not have sufficient
-- permissions to invoke the action, or the parameter values fall outside the
-- specified constraints, the action fails by throwing OperationNotPermitted.
-- For details and example IAM policies, see Using IAM to Manage Access to
-- Amazon SWF Workflows. StartWorkflowExecution Example POST / HTTP/1.1 Host:
-- swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U; Windows NT
-- 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET CLR
-- 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Sat, 14 Jan 2012 22:45:13 GMT X-Amz-Target:
-- SimpleWorkflowService.StartWorkflowExecution Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=aYxuqLX+TO91kPVg+jh+aA8PWxQazQRN2+SZUGdOgU0=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 417 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "workflowId": "20110927-T-1", "workflowType": {"name":
-- "customerOrderWorkflow", "version": "1.0"}, "taskList": {"name":
-- "specialTaskList"}, "input":
-- "arbitrary-string-that-is-meaningful-to-the-workflow",
-- "executionStartToCloseTimeout": "1800", "tagList": ["music purchase",
-- "digital", "ricoh-the-dog"], "taskStartToCloseTimeout": "600",
-- "childPolicy": "TERMINATE"} HTTP/1.1 200 OK Content-Length: 48
-- Content-Type: application/json x-amzn-RequestId:
-- 6c25f6e6-3f01-11e1-9a27-0760db01a4a8
-- {"runId":"1e536162-f1ea-48b0-85f3-aade88eef2f7"}.
--
-- See: 'Network.AWS.SWF.V2012_01_25.StartWorkflowExecution'
startWorkflowExecution :: ( MonadCatch m
                          , MonadResource m
                          , MonadError Error m
                          , MonadReader Env m
                          , AWSRequest a
                          )
                       => Text -- ^ 'swe1Domain'
                       -> Text -- ^ 'swe1WorkflowId'
                       -> WorkflowType -- ^ 'swe1WorkflowType'
                       -> State StartWorkflowExecution a
                       -> m StartWorkflowExecutionResponse
startWorkflowExecution p1 p2 p3 s =
    send $ (mkStartWorkflowExecution p1 p2 p3) &~ s

-- | Records a WorkflowExecutionTerminated event and forces closure of the
-- workflow execution identified by the given domain, runId, and workflowId.
-- The child policy, registered with the workflow type or specified when
-- starting this execution, is applied to any open child workflow executions
-- of this workflow execution. If the identified workflow execution was in
-- progress, it is terminated immediately. If a runId is not specified, then
-- the WorkflowExecutionTerminated event is recorded in the history of the
-- current open workflow with the matching workflowId in the domain. You
-- should consider using RequestCancelWorkflowExecution action instead because
-- it allows the workflow to gracefully close while TerminateWorkflowExecution
-- does not. Access Control You can use IAM policies to control this action's
-- access to Amazon SWF resources as follows: Use a Resource element with the
-- domain name to limit the action to only specified domains. Use an Action
-- element to allow or deny permission to call this action. You cannot use an
-- IAM policy to constrain this action's parameters. If the caller does not
-- have sufficient permissions to invoke the action, or the parameter values
-- fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. TerminateWorkflowExecution
-- Example POST / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent:
-- Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 04:56:34 GMT
-- X-Amz-Target: SimpleWorkflowService.TerminateWorkflowExecution
-- Content-Encoding: amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=JHMRAjN6JGPawEuhiANHfiCil9KOGfDF/cuXYmuu9S4=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 218 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "workflowId": "20110927-T-1", "runId":
-- "94861fda-a714-4126-95d7-55ba847da8ab", "reason": "transaction canceled",
-- "details": "customer credit card declined", "childPolicy": "TERMINATE"}
-- HTTP/1.1 200 OK Content-Length: 0 Content-Type: application/json
-- x-amzn-RequestId: 76d68a47-3ffe-11e1-b118-3bfa5e8e7fc3.
--
-- See: 'Network.AWS.SWF.V2012_01_25.TerminateWorkflowExecution'
terminateWorkflowExecution :: ( MonadCatch m
                              , MonadResource m
                              , MonadError Error m
                              , MonadReader Env m
                              , AWSRequest a
                              )
                           => Text -- ^ 'tweDomain'
                           -> Text -- ^ 'tweWorkflowId'
                           -> State TerminateWorkflowExecution a
                           -> m TerminateWorkflowExecutionResponse
terminateWorkflowExecution p1 p2 s =
    send $ (mkTerminateWorkflowExecution p1 p2) &~ s
