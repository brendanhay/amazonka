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
-- Module      : Network.AWS.KMS.ListRetirableGrants
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all grants for which the grant's @RetiringPrincipal@ matches the one specified.
--
--
-- A typical use is to list all grants that you are able to retire. To retire a grant, use 'RetireGrant' .
--
module Network.AWS.KMS.ListRetirableGrants
    (
    -- * Creating a Request
      listRetirableGrants
    , ListRetirableGrants
    -- * Request Lenses
    , lrgMarker
    , lrgLimit
    , lrgRetiringPrincipal

    -- * Destructuring the Response
    , listGrantsResponse
    , ListGrantsResponse
    -- * Response Lenses
    , lgTruncated
    , lgGrants
    , lgNextMarker
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listRetirableGrants' smart constructor.
data ListRetirableGrants = ListRetirableGrants'
  { _lrgMarker            :: !(Maybe Text)
  , _lrgLimit             :: !(Maybe Nat)
  , _lrgRetiringPrincipal :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRetirableGrants' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrgMarker' - Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
--
-- * 'lrgLimit' - Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer. This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
--
-- * 'lrgRetiringPrincipal' - The retiring principal for which to list grants. To specify the retiring principal, use the <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, federated users, and assumed role users. For examples of the ARN syntax for specifying a principal, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /Amazon Web Services General Reference/ .
listRetirableGrants
    :: Text -- ^ 'lrgRetiringPrincipal'
    -> ListRetirableGrants
listRetirableGrants pRetiringPrincipal_ =
  ListRetirableGrants'
    { _lrgMarker = Nothing
    , _lrgLimit = Nothing
    , _lrgRetiringPrincipal = pRetiringPrincipal_
    }


-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received.
lrgMarker :: Lens' ListRetirableGrants (Maybe Text)
lrgMarker = lens _lrgMarker (\ s a -> s{_lrgMarker = a})

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer. This value is optional. If you include a value, it must be between 1 and 100, inclusive. If you do not include a value, it defaults to 50.
lrgLimit :: Lens' ListRetirableGrants (Maybe Natural)
lrgLimit = lens _lrgLimit (\ s a -> s{_lrgLimit = a}) . mapping _Nat

-- | The retiring principal for which to list grants. To specify the retiring principal, use the <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an AWS principal. Valid AWS principals include AWS accounts (root), IAM users, federated users, and assumed role users. For examples of the ARN syntax for specifying a principal, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-iam AWS Identity and Access Management (IAM)> in the Example ARNs section of the /Amazon Web Services General Reference/ .
lrgRetiringPrincipal :: Lens' ListRetirableGrants Text
lrgRetiringPrincipal = lens _lrgRetiringPrincipal (\ s a -> s{_lrgRetiringPrincipal = a})

instance AWSRequest ListRetirableGrants where
        type Rs ListRetirableGrants = ListGrantsResponse
        request = postJSON kms
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable ListRetirableGrants where

instance NFData ListRetirableGrants where

instance ToHeaders ListRetirableGrants where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.ListRetirableGrants" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListRetirableGrants where
        toJSON ListRetirableGrants'{..}
          = object
              (catMaybes
                 [("Marker" .=) <$> _lrgMarker,
                  ("Limit" .=) <$> _lrgLimit,
                  Just ("RetiringPrincipal" .= _lrgRetiringPrincipal)])

instance ToPath ListRetirableGrants where
        toPath = const "/"

instance ToQuery ListRetirableGrants where
        toQuery = const mempty
