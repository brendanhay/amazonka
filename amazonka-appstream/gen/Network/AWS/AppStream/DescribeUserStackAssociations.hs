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
-- Module      : Network.AWS.AppStream.DescribeUserStackAssociations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the UserStackAssociation objects. You must specify either or both of the following:
--
--
--     * The stack name
--
--     * The user name (email address of the user associated with the stack) and the authentication type for the user
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeUserStackAssociations
    (
    -- * Creating a Request
      describeUserStackAssociations
    , DescribeUserStackAssociations
    -- * Request Lenses
    , dusaUserName
    , dusaNextToken
    , dusaAuthenticationType
    , dusaMaxResults
    , dusaStackName

    -- * Destructuring the Response
    , describeUserStackAssociationsResponse
    , DescribeUserStackAssociationsResponse
    -- * Response Lenses
    , dusarsUserStackAssociations
    , dusarsNextToken
    , dusarsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeUserStackAssociations' smart constructor.
data DescribeUserStackAssociations = DescribeUserStackAssociations'
  { _dusaUserName           :: !(Maybe (Sensitive Text))
  , _dusaNextToken          :: !(Maybe Text)
  , _dusaAuthenticationType :: !(Maybe AuthenticationType)
  , _dusaMaxResults         :: !(Maybe Nat)
  , _dusaStackName          :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUserStackAssociations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dusaUserName' - The email address of the user who is associated with the stack.
--
-- * 'dusaNextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- * 'dusaAuthenticationType' - The authentication type for the user who is associated with the stack. You must specify USERPOOL.
--
-- * 'dusaMaxResults' - The maximum size of each page of results.
--
-- * 'dusaStackName' - The name of the stack that is associated with the user.
describeUserStackAssociations
    :: DescribeUserStackAssociations
describeUserStackAssociations =
  DescribeUserStackAssociations'
    { _dusaUserName = Nothing
    , _dusaNextToken = Nothing
    , _dusaAuthenticationType = Nothing
    , _dusaMaxResults = Nothing
    , _dusaStackName = Nothing
    }


-- | The email address of the user who is associated with the stack.
dusaUserName :: Lens' DescribeUserStackAssociations (Maybe Text)
dusaUserName = lens _dusaUserName (\ s a -> s{_dusaUserName = a}) . mapping _Sensitive

-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
dusaNextToken :: Lens' DescribeUserStackAssociations (Maybe Text)
dusaNextToken = lens _dusaNextToken (\ s a -> s{_dusaNextToken = a})

-- | The authentication type for the user who is associated with the stack. You must specify USERPOOL.
dusaAuthenticationType :: Lens' DescribeUserStackAssociations (Maybe AuthenticationType)
dusaAuthenticationType = lens _dusaAuthenticationType (\ s a -> s{_dusaAuthenticationType = a})

-- | The maximum size of each page of results.
dusaMaxResults :: Lens' DescribeUserStackAssociations (Maybe Natural)
dusaMaxResults = lens _dusaMaxResults (\ s a -> s{_dusaMaxResults = a}) . mapping _Nat

-- | The name of the stack that is associated with the user.
dusaStackName :: Lens' DescribeUserStackAssociations (Maybe Text)
dusaStackName = lens _dusaStackName (\ s a -> s{_dusaStackName = a})

instance AWSPager DescribeUserStackAssociations where
        page rq rs
          | stop (rs ^. dusarsNextToken) = Nothing
          | stop (rs ^. dusarsUserStackAssociations) = Nothing
          | otherwise =
            Just $ rq & dusaNextToken .~ rs ^. dusarsNextToken

instance AWSRequest DescribeUserStackAssociations
         where
        type Rs DescribeUserStackAssociations =
             DescribeUserStackAssociationsResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 DescribeUserStackAssociationsResponse' <$>
                   (x .?> "UserStackAssociations" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeUserStackAssociations where

instance NFData DescribeUserStackAssociations where

instance ToHeaders DescribeUserStackAssociations
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.DescribeUserStackAssociations"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeUserStackAssociations where
        toJSON DescribeUserStackAssociations'{..}
          = object
              (catMaybes
                 [("UserName" .=) <$> _dusaUserName,
                  ("NextToken" .=) <$> _dusaNextToken,
                  ("AuthenticationType" .=) <$>
                    _dusaAuthenticationType,
                  ("MaxResults" .=) <$> _dusaMaxResults,
                  ("StackName" .=) <$> _dusaStackName])

instance ToPath DescribeUserStackAssociations where
        toPath = const "/"

instance ToQuery DescribeUserStackAssociations where
        toQuery = const mempty

-- | /See:/ 'describeUserStackAssociationsResponse' smart constructor.
data DescribeUserStackAssociationsResponse = DescribeUserStackAssociationsResponse'
  { _dusarsUserStackAssociations :: !(Maybe [UserStackAssociation])
  , _dusarsNextToken             :: !(Maybe Text)
  , _dusarsResponseStatus        :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUserStackAssociationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dusarsUserStackAssociations' - The UserStackAssociation objects.
--
-- * 'dusarsNextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- * 'dusarsResponseStatus' - -- | The response status code.
describeUserStackAssociationsResponse
    :: Int -- ^ 'dusarsResponseStatus'
    -> DescribeUserStackAssociationsResponse
describeUserStackAssociationsResponse pResponseStatus_ =
  DescribeUserStackAssociationsResponse'
    { _dusarsUserStackAssociations = Nothing
    , _dusarsNextToken = Nothing
    , _dusarsResponseStatus = pResponseStatus_
    }


-- | The UserStackAssociation objects.
dusarsUserStackAssociations :: Lens' DescribeUserStackAssociationsResponse [UserStackAssociation]
dusarsUserStackAssociations = lens _dusarsUserStackAssociations (\ s a -> s{_dusarsUserStackAssociations = a}) . _Default . _Coerce

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
dusarsNextToken :: Lens' DescribeUserStackAssociationsResponse (Maybe Text)
dusarsNextToken = lens _dusarsNextToken (\ s a -> s{_dusarsNextToken = a})

-- | -- | The response status code.
dusarsResponseStatus :: Lens' DescribeUserStackAssociationsResponse Int
dusarsResponseStatus = lens _dusarsResponseStatus (\ s a -> s{_dusarsResponseStatus = a})

instance NFData DescribeUserStackAssociationsResponse
         where
