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
-- Module      : Network.AWS.IAM.ListPoliciesGrantingServiceAccess
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of policies that the IAM identity (user, group, or role) can use to access each specified service.
--
--
-- The list of policies returned by the operation depends on the ARN of the identity that you provide.
--
--     * __User__ – The list of policies includes the managed and inline policies that are attached to the user directly. The list also includes any additional managed and inline policies that are attached to the group to which the user belongs.
--
--     * __Group__ – The list of policies includes only the managed and inline policies that are attached to the group directly. Policies that are attached to the group’s user are not included.
--
--     * __Role__ – The list of policies includes only the managed and inline policies that are attached to the role.
--
--
--
-- For each managed policy, this operation returns the ARN and policy name. For each inline policy, it returns the policy name and the entity to which it is attached. Inline policies do not have an ARN. For more information about these policy types, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- Policies that are attached to users and roles as permissions boundaries are not returned. To view which managed policy is currently used to set the permissions boundary for a user or role, use the 'GetUser' or 'GetRole' operations.
--
module Network.AWS.IAM.ListPoliciesGrantingServiceAccess
    (
    -- * Creating a Request
      listPoliciesGrantingServiceAccess
    , ListPoliciesGrantingServiceAccess
    -- * Request Lenses
    , lpgsaMarker
    , lpgsaARN
    , lpgsaServiceNamespaces

    -- * Destructuring the Response
    , listPoliciesGrantingServiceAccessResponse
    , ListPoliciesGrantingServiceAccessResponse
    -- * Response Lenses
    , lpgsarsMarker
    , lpgsarsIsTruncated
    , lpgsarsResponseStatus
    , lpgsarsPoliciesGrantingServiceAccess
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listPoliciesGrantingServiceAccess' smart constructor.
data ListPoliciesGrantingServiceAccess = ListPoliciesGrantingServiceAccess'
  { _lpgsaMarker            :: !(Maybe Text)
  , _lpgsaARN               :: !Text
  , _lpgsaServiceNamespaces :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPoliciesGrantingServiceAccess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpgsaMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- * 'lpgsaARN' - The ARN of the IAM identity (user, group, or role) whose policies you want to list.
--
-- * 'lpgsaServiceNamespaces' - The service namespace for the AWS services whose policies you want to list. To learn the service namespace for a service, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
listPoliciesGrantingServiceAccess
    :: Text -- ^ 'lpgsaARN'
    -> NonEmpty Text -- ^ 'lpgsaServiceNamespaces'
    -> ListPoliciesGrantingServiceAccess
listPoliciesGrantingServiceAccess pARN_ pServiceNamespaces_ =
  ListPoliciesGrantingServiceAccess'
    { _lpgsaMarker = Nothing
    , _lpgsaARN = pARN_
    , _lpgsaServiceNamespaces = _List1 # pServiceNamespaces_
    }


-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
lpgsaMarker :: Lens' ListPoliciesGrantingServiceAccess (Maybe Text)
lpgsaMarker = lens _lpgsaMarker (\ s a -> s{_lpgsaMarker = a})

-- | The ARN of the IAM identity (user, group, or role) whose policies you want to list.
lpgsaARN :: Lens' ListPoliciesGrantingServiceAccess Text
lpgsaARN = lens _lpgsaARN (\ s a -> s{_lpgsaARN = a})

-- | The service namespace for the AWS services whose policies you want to list. To learn the service namespace for a service, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
lpgsaServiceNamespaces :: Lens' ListPoliciesGrantingServiceAccess (NonEmpty Text)
lpgsaServiceNamespaces = lens _lpgsaServiceNamespaces (\ s a -> s{_lpgsaServiceNamespaces = a}) . _List1

instance AWSRequest ListPoliciesGrantingServiceAccess
         where
        type Rs ListPoliciesGrantingServiceAccess =
             ListPoliciesGrantingServiceAccessResponse
        request = postQuery iam
        response
          = receiveXMLWrapper
              "ListPoliciesGrantingServiceAccessResult"
              (\ s h x ->
                 ListPoliciesGrantingServiceAccessResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "PoliciesGrantingServiceAccess" .!@ mempty >>=
                        parseXMLList "member"))

instance Hashable ListPoliciesGrantingServiceAccess
         where

instance NFData ListPoliciesGrantingServiceAccess
         where

instance ToHeaders ListPoliciesGrantingServiceAccess
         where
        toHeaders = const mempty

instance ToPath ListPoliciesGrantingServiceAccess
         where
        toPath = const "/"

instance ToQuery ListPoliciesGrantingServiceAccess
         where
        toQuery ListPoliciesGrantingServiceAccess'{..}
          = mconcat
              ["Action" =:
                 ("ListPoliciesGrantingServiceAccess" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Marker" =: _lpgsaMarker, "Arn" =: _lpgsaARN,
               "ServiceNamespaces" =:
                 toQueryList "member" _lpgsaServiceNamespaces]

-- | /See:/ 'listPoliciesGrantingServiceAccessResponse' smart constructor.
data ListPoliciesGrantingServiceAccessResponse = ListPoliciesGrantingServiceAccessResponse'
  { _lpgsarsMarker :: !(Maybe Text)
  , _lpgsarsIsTruncated :: !(Maybe Bool)
  , _lpgsarsResponseStatus :: !Int
  , _lpgsarsPoliciesGrantingServiceAccess :: ![ListPoliciesGrantingServiceAccessEntry]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPoliciesGrantingServiceAccessResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpgsarsMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'lpgsarsIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- * 'lpgsarsResponseStatus' - -- | The response status code.
--
-- * 'lpgsarsPoliciesGrantingServiceAccess' - A @ListPoliciesGrantingServiceAccess@ object that contains details about the permissions policies attached to the specified identity (user, group, or role).
listPoliciesGrantingServiceAccessResponse
    :: Int -- ^ 'lpgsarsResponseStatus'
    -> ListPoliciesGrantingServiceAccessResponse
listPoliciesGrantingServiceAccessResponse pResponseStatus_ =
  ListPoliciesGrantingServiceAccessResponse'
    { _lpgsarsMarker = Nothing
    , _lpgsarsIsTruncated = Nothing
    , _lpgsarsResponseStatus = pResponseStatus_
    , _lpgsarsPoliciesGrantingServiceAccess = mempty
    }


-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
lpgsarsMarker :: Lens' ListPoliciesGrantingServiceAccessResponse (Maybe Text)
lpgsarsMarker = lens _lpgsarsMarker (\ s a -> s{_lpgsarsMarker = a})

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
lpgsarsIsTruncated :: Lens' ListPoliciesGrantingServiceAccessResponse (Maybe Bool)
lpgsarsIsTruncated = lens _lpgsarsIsTruncated (\ s a -> s{_lpgsarsIsTruncated = a})

-- | -- | The response status code.
lpgsarsResponseStatus :: Lens' ListPoliciesGrantingServiceAccessResponse Int
lpgsarsResponseStatus = lens _lpgsarsResponseStatus (\ s a -> s{_lpgsarsResponseStatus = a})

-- | A @ListPoliciesGrantingServiceAccess@ object that contains details about the permissions policies attached to the specified identity (user, group, or role).
lpgsarsPoliciesGrantingServiceAccess :: Lens' ListPoliciesGrantingServiceAccessResponse [ListPoliciesGrantingServiceAccessEntry]
lpgsarsPoliciesGrantingServiceAccess = lens _lpgsarsPoliciesGrantingServiceAccess (\ s a -> s{_lpgsarsPoliciesGrantingServiceAccess = a}) . _Coerce

instance NFData
           ListPoliciesGrantingServiceAccessResponse
         where
