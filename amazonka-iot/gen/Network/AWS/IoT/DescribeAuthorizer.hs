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
-- Module      : Network.AWS.IoT.DescribeAuthorizer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an authorizer.
--
--
module Network.AWS.IoT.DescribeAuthorizer
    (
    -- * Creating a Request
      describeAuthorizer
    , DescribeAuthorizer
    -- * Request Lenses
    , daAuthorizerName

    -- * Destructuring the Response
    , describeAuthorizerResponse
    , DescribeAuthorizerResponse
    -- * Response Lenses
    , darsAuthorizerDescription
    , darsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeAuthorizer' smart constructor.
newtype DescribeAuthorizer = DescribeAuthorizer'
  { _daAuthorizerName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAuthorizerName' - The name of the authorizer to describe.
describeAuthorizer
    :: Text -- ^ 'daAuthorizerName'
    -> DescribeAuthorizer
describeAuthorizer pAuthorizerName_ =
  DescribeAuthorizer' {_daAuthorizerName = pAuthorizerName_}


-- | The name of the authorizer to describe.
daAuthorizerName :: Lens' DescribeAuthorizer Text
daAuthorizerName = lens _daAuthorizerName (\ s a -> s{_daAuthorizerName = a})

instance AWSRequest DescribeAuthorizer where
        type Rs DescribeAuthorizer =
             DescribeAuthorizerResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAuthorizerResponse' <$>
                   (x .?> "authorizerDescription") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeAuthorizer where

instance NFData DescribeAuthorizer where

instance ToHeaders DescribeAuthorizer where
        toHeaders = const mempty

instance ToPath DescribeAuthorizer where
        toPath DescribeAuthorizer'{..}
          = mconcat ["/authorizer/", toBS _daAuthorizerName]

instance ToQuery DescribeAuthorizer where
        toQuery = const mempty

-- | /See:/ 'describeAuthorizerResponse' smart constructor.
data DescribeAuthorizerResponse = DescribeAuthorizerResponse'
  { _darsAuthorizerDescription :: !(Maybe AuthorizerDescription)
  , _darsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAuthorizerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsAuthorizerDescription' - The authorizer description.
--
-- * 'darsResponseStatus' - -- | The response status code.
describeAuthorizerResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DescribeAuthorizerResponse
describeAuthorizerResponse pResponseStatus_ =
  DescribeAuthorizerResponse'
    { _darsAuthorizerDescription = Nothing
    , _darsResponseStatus = pResponseStatus_
    }


-- | The authorizer description.
darsAuthorizerDescription :: Lens' DescribeAuthorizerResponse (Maybe AuthorizerDescription)
darsAuthorizerDescription = lens _darsAuthorizerDescription (\ s a -> s{_darsAuthorizerDescription = a})

-- | -- | The response status code.
darsResponseStatus :: Lens' DescribeAuthorizerResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

instance NFData DescribeAuthorizerResponse where
