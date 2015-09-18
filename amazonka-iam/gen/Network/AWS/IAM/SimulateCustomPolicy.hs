{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.SimulateCustomPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulate a set of IAM policies against a list of API actions and AWS
-- resources to determine the policies\' effective permissions. The
-- policies are provided as a list of strings.
--
-- The simulation does not perform the API actions, it only checks the
-- authorization to determine if the simulated policies allow or deny the
-- actions.
--
-- If you want to simulate existing policies attached to an IAM user,
-- group, or role, use SimulatePrincipalPolicy instead.
--
-- Context keys are variables maintained by AWS and its services that
-- provide details about the context of an API query request, and can be
-- evaluated by using the 'Condition' element of an IAM policy. To get the
-- list of context keys required by the policies to simulate them
-- correctly, use GetContextKeysForCustomPolicy.
--
-- If the output is long, you can paginate the results using the 'MaxItems'
-- and 'Marker' parameters.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_SimulateCustomPolicy.html AWS API Reference> for SimulateCustomPolicy.
module Network.AWS.IAM.SimulateCustomPolicy
    (
    -- * Creating a Request
      simulateCustomPolicy
    , SimulateCustomPolicy
    -- * Request Lenses
    , scpResourceARNs
    , scpMarker
    , scpMaxItems
    , scpContextEntries
    , scpPolicyInputList
    , scpActionNames

    -- * Destructuring the Response
    , simulatePolicyResponse
    , SimulatePolicyResponse
    -- * Response Lenses
    , spEvaluationResults
    , spMarker
    , spIsTruncated
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'simulateCustomPolicy' smart constructor.
data SimulateCustomPolicy = SimulateCustomPolicy'
    { _scpResourceARNs    :: !(Maybe [Text])
    , _scpMarker          :: !(Maybe Text)
    , _scpMaxItems        :: !(Maybe Nat)
    , _scpContextEntries  :: !(Maybe [ContextEntry])
    , _scpPolicyInputList :: ![Text]
    , _scpActionNames     :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SimulateCustomPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scpResourceARNs'
--
-- * 'scpMarker'
--
-- * 'scpMaxItems'
--
-- * 'scpContextEntries'
--
-- * 'scpPolicyInputList'
--
-- * 'scpActionNames'
simulateCustomPolicy
    :: SimulateCustomPolicy
simulateCustomPolicy =
    SimulateCustomPolicy'
    { _scpResourceARNs = Nothing
    , _scpMarker = Nothing
    , _scpMaxItems = Nothing
    , _scpContextEntries = Nothing
    , _scpPolicyInputList = mempty
    , _scpActionNames = mempty
    }

-- | A list of ARNs of AWS resources to include in the simulation. If this
-- parameter is not provided then the value defaults to '*' (all
-- resources). Each API in the 'ActionNames' parameter is evaluated for
-- each resource in this list. The simulation determines the access result
-- (allowed or denied) of each combination and reports it in the response.
scpResourceARNs :: Lens' SimulateCustomPolicy [Text]
scpResourceARNs = lens _scpResourceARNs (\ s a -> s{_scpResourceARNs = a}) . _Default . _Coerce;

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the 'Marker' element in the response you received to inform
-- the next call about where to start.
scpMarker :: Lens' SimulateCustomPolicy (Maybe Text)
scpMarker = lens _scpMarker (\ s a -> s{_scpMarker = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. If this is the case, the 'IsTruncated' response
-- element returns 'true' and 'Marker' contains a value to include in the
-- subsequent call that tells the service where to continue from.
scpMaxItems :: Lens' SimulateCustomPolicy (Maybe Natural)
scpMaxItems = lens _scpMaxItems (\ s a -> s{_scpMaxItems = a}) . mapping _Nat;

-- | A list of context keys and corresponding values that are used by the
-- simulation. Whenever a context key is evaluated by a 'Condition' element
-- in one of the simulated IAM permission policies, the corresponding value
-- is supplied.
scpContextEntries :: Lens' SimulateCustomPolicy [ContextEntry]
scpContextEntries = lens _scpContextEntries (\ s a -> s{_scpContextEntries = a}) . _Default . _Coerce;

-- | A list of policy documents to include in the simulation. Each document
-- is specified as a string containing the complete, valid JSON text of an
-- IAM policy.
scpPolicyInputList :: Lens' SimulateCustomPolicy [Text]
scpPolicyInputList = lens _scpPolicyInputList (\ s a -> s{_scpPolicyInputList = a}) . _Coerce;

-- | A list of names of API actions to evaluate in the simulation. Each
-- action is evaluated for each resource. Each action must include the
-- service identifier, such as 'iam:CreateUser'.
scpActionNames :: Lens' SimulateCustomPolicy [Text]
scpActionNames = lens _scpActionNames (\ s a -> s{_scpActionNames = a}) . _Coerce;

instance AWSRequest SimulateCustomPolicy where
        type Rs SimulateCustomPolicy = SimulatePolicyResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "SimulateCustomPolicyResult"
              (\ s h x -> parseXML x)

instance ToHeaders SimulateCustomPolicy where
        toHeaders = const mempty

instance ToPath SimulateCustomPolicy where
        toPath = const "/"

instance ToQuery SimulateCustomPolicy where
        toQuery SimulateCustomPolicy'{..}
          = mconcat
              ["Action" =: ("SimulateCustomPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "ResourceArns" =:
                 toQuery (toQueryList "member" <$> _scpResourceARNs),
               "Marker" =: _scpMarker, "MaxItems" =: _scpMaxItems,
               "ContextEntries" =:
                 toQuery
                   (toQueryList "member" <$> _scpContextEntries),
               "PolicyInputList" =:
                 toQueryList "member" _scpPolicyInputList,
               "ActionNames" =:
                 toQueryList "member" _scpActionNames]
