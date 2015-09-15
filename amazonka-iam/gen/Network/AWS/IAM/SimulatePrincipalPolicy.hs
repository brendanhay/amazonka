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
-- Module      : Network.AWS.IAM.SimulatePrincipalPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulate the set of IAM policies attached to an IAM entity against a
-- list of API actions and AWS resources to determine the policies\'
-- effective permissions. The entity can be an IAM user, group, or role. If
-- you specify a user, then the simulation also includes all of the
-- policies attached to groups that the user is a member of.
--
-- You can optionally include a list of one or more additional policies
-- specified as strings to include in the simulation. If you want to
-- simulate only policies specified as strings, use SimulateCustomPolicy
-- instead.
--
-- The simulation does not perform the API actions, it only checks the
-- authorization to determine if the simulated policies allow or deny the
-- actions.
--
-- __Note:__ This API discloses information about the permissions granted
-- to other users. If you do not want users to see other user\'s
-- permissions, then consider allowing them to use SimulateCustomPolicy
-- instead.
--
-- Context keys are variables maintained by AWS and its services that
-- provide details about the context of an API query request, and can be
-- evaluated by using the 'Condition' element of an IAM policy. To get the
-- list of context keys required by the policies to simulate them
-- correctly, use GetContextKeysForPrincipalPolicy.
--
-- If the output is long, you can paginate the results using the 'MaxItems'
-- and 'Marker' parameters.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_SimulatePrincipalPolicy.html AWS API Reference> for SimulatePrincipalPolicy.
module Network.AWS.IAM.SimulatePrincipalPolicy
    (
    -- * Creating a Request
      simulatePrincipalPolicy
    , SimulatePrincipalPolicy
    -- * Request Lenses
    , sppPolicyInputList
    , sppResourceARNs
    , sppMarker
    , sppMaxItems
    , sppContextEntries
    , sppPolicySourceARN
    , sppActionNames

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

-- | /See:/ 'simulatePrincipalPolicy' smart constructor.
data SimulatePrincipalPolicy = SimulatePrincipalPolicy'
    { _sppPolicyInputList :: !(Maybe [Text])
    , _sppResourceARNs    :: !(Maybe [Text])
    , _sppMarker          :: !(Maybe Text)
    , _sppMaxItems        :: !(Maybe Nat)
    , _sppContextEntries  :: !(Maybe [ContextEntry])
    , _sppPolicySourceARN :: !Text
    , _sppActionNames     :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SimulatePrincipalPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sppPolicyInputList'
--
-- * 'sppResourceARNs'
--
-- * 'sppMarker'
--
-- * 'sppMaxItems'
--
-- * 'sppContextEntries'
--
-- * 'sppPolicySourceARN'
--
-- * 'sppActionNames'
simulatePrincipalPolicy
    :: Text -- ^ 'sppPolicySourceARN'
    -> SimulatePrincipalPolicy
simulatePrincipalPolicy pPolicySourceARN_ =
    SimulatePrincipalPolicy'
    { _sppPolicyInputList = Nothing
    , _sppResourceARNs = Nothing
    , _sppMarker = Nothing
    , _sppMaxItems = Nothing
    , _sppContextEntries = Nothing
    , _sppPolicySourceARN = pPolicySourceARN_
    , _sppActionNames = mempty
    }

-- | An optional list of additional policy documents to include in the
-- simulation. Each document is specified as a string containing the
-- complete, valid JSON text of an IAM policy.
sppPolicyInputList :: Lens' SimulatePrincipalPolicy [Text]
sppPolicyInputList = lens _sppPolicyInputList (\ s a -> s{_sppPolicyInputList = a}) . _Default . _Coerce;

-- | A list of ARNs of AWS resources to include in the simulation. If this
-- parameter is not provided then the value defaults to '*' (all
-- resources). Each API in the 'ActionNames' parameter is evaluated for
-- each resource in this list. The simulation determines the access result
-- (allowed or denied) of each combination and reports it in the response.
sppResourceARNs :: Lens' SimulatePrincipalPolicy [Text]
sppResourceARNs = lens _sppResourceARNs (\ s a -> s{_sppResourceARNs = a}) . _Default . _Coerce;

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the 'Marker' element in the response you received to inform
-- the next call about where to start.
sppMarker :: Lens' SimulatePrincipalPolicy (Maybe Text)
sppMarker = lens _sppMarker (\ s a -> s{_sppMarker = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. If this is the case, the 'IsTruncated' response
-- element returns 'true' and 'Marker' contains a value to include in the
-- subsequent call that tells the service where to continue from.
sppMaxItems :: Lens' SimulatePrincipalPolicy (Maybe Natural)
sppMaxItems = lens _sppMaxItems (\ s a -> s{_sppMaxItems = a}) . mapping _Nat;

-- | A list of context keys and corresponding values that are used by the
-- simulation. Whenever a context key is evaluated by a 'Condition' element
-- in one of the simulated IAM permission policies, the corresponding value
-- is supplied.
sppContextEntries :: Lens' SimulatePrincipalPolicy [ContextEntry]
sppContextEntries = lens _sppContextEntries (\ s a -> s{_sppContextEntries = a}) . _Default . _Coerce;

-- | The Amazon Resource Name (ARN) of a user, group, or role whose policies
-- you want to include in the simulation. If you specify a user, group, or
-- role, the simulation includes all policies associated with that entity.
-- If you specify a user, the simulation also includes all policies
-- attached to any groups the user is a member of.
sppPolicySourceARN :: Lens' SimulatePrincipalPolicy Text
sppPolicySourceARN = lens _sppPolicySourceARN (\ s a -> s{_sppPolicySourceARN = a});

-- | A list of names of API actions to evaluate in the simulation. Each
-- action is evaluated for each resource. Each action must include the
-- service identifier, such as 'iam:CreateUser'.
sppActionNames :: Lens' SimulatePrincipalPolicy [Text]
sppActionNames = lens _sppActionNames (\ s a -> s{_sppActionNames = a}) . _Coerce;

instance AWSRequest SimulatePrincipalPolicy where
        type Rs SimulatePrincipalPolicy =
             SimulatePolicyResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "SimulatePrincipalPolicyResult"
              (\ s h x -> parseXML x)

instance ToHeaders SimulatePrincipalPolicy where
        toHeaders = const mempty

instance ToPath SimulatePrincipalPolicy where
        toPath = const "/"

instance ToQuery SimulatePrincipalPolicy where
        toQuery SimulatePrincipalPolicy'{..}
          = mconcat
              ["Action" =:
                 ("SimulatePrincipalPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PolicyInputList" =:
                 toQuery
                   (toQueryList "member" <$> _sppPolicyInputList),
               "ResourceArns" =:
                 toQuery (toQueryList "member" <$> _sppResourceARNs),
               "Marker" =: _sppMarker, "MaxItems" =: _sppMaxItems,
               "ContextEntries" =:
                 toQuery
                   (toQueryList "member" <$> _sppContextEntries),
               "PolicySourceArn" =: _sppPolicySourceARN,
               "ActionNames" =:
                 toQueryList "member" _sppActionNames]
