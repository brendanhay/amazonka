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
-- Module      : Network.AWS.IoT.DescribeDefaultAuthorizer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the default authorizer.
--
--
module Network.AWS.IoT.DescribeDefaultAuthorizer
    (
    -- * Creating a Request
      describeDefaultAuthorizer
    , DescribeDefaultAuthorizer

    -- * Destructuring the Response
    , describeDefaultAuthorizerResponse
    , DescribeDefaultAuthorizerResponse
    -- * Response Lenses
    , ddarsAuthorizerDescription
    , ddarsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDefaultAuthorizer' smart constructor.
data DescribeDefaultAuthorizer =
  DescribeDefaultAuthorizer'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDefaultAuthorizer' with the minimum fields required to make a request.
--
describeDefaultAuthorizer
    :: DescribeDefaultAuthorizer
describeDefaultAuthorizer = DescribeDefaultAuthorizer'


instance AWSRequest DescribeDefaultAuthorizer where
        type Rs DescribeDefaultAuthorizer =
             DescribeDefaultAuthorizerResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDefaultAuthorizerResponse' <$>
                   (x .?> "authorizerDescription") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeDefaultAuthorizer where

instance NFData DescribeDefaultAuthorizer where

instance ToHeaders DescribeDefaultAuthorizer where
        toHeaders = const mempty

instance ToPath DescribeDefaultAuthorizer where
        toPath = const "/default-authorizer"

instance ToQuery DescribeDefaultAuthorizer where
        toQuery = const mempty

-- | /See:/ 'describeDefaultAuthorizerResponse' smart constructor.
data DescribeDefaultAuthorizerResponse = DescribeDefaultAuthorizerResponse'
  { _ddarsAuthorizerDescription :: !(Maybe AuthorizerDescription)
  , _ddarsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDefaultAuthorizerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddarsAuthorizerDescription' - The default authorizer's description.
--
-- * 'ddarsResponseStatus' - -- | The response status code.
describeDefaultAuthorizerResponse
    :: Int -- ^ 'ddarsResponseStatus'
    -> DescribeDefaultAuthorizerResponse
describeDefaultAuthorizerResponse pResponseStatus_ =
  DescribeDefaultAuthorizerResponse'
    { _ddarsAuthorizerDescription = Nothing
    , _ddarsResponseStatus = pResponseStatus_
    }


-- | The default authorizer's description.
ddarsAuthorizerDescription :: Lens' DescribeDefaultAuthorizerResponse (Maybe AuthorizerDescription)
ddarsAuthorizerDescription = lens _ddarsAuthorizerDescription (\ s a -> s{_ddarsAuthorizerDescription = a})

-- | -- | The response status code.
ddarsResponseStatus :: Lens' DescribeDefaultAuthorizerResponse Int
ddarsResponseStatus = lens _ddarsResponseStatus (\ s a -> s{_ddarsResponseStatus = a})

instance NFData DescribeDefaultAuthorizerResponse
         where
