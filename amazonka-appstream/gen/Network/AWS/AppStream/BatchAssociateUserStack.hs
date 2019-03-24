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
-- Module      : Network.AWS.AppStream.BatchAssociateUserStack
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified users with the specified stacks. Users in a user pool cannot be assigned to stacks with fleets that are joined to an Active Directory domain.
--
--
module Network.AWS.AppStream.BatchAssociateUserStack
    (
    -- * Creating a Request
      batchAssociateUserStack
    , BatchAssociateUserStack
    -- * Request Lenses
    , bausUserStackAssociations

    -- * Destructuring the Response
    , batchAssociateUserStackResponse
    , BatchAssociateUserStackResponse
    -- * Response Lenses
    , bausrsErrors
    , bausrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchAssociateUserStack' smart constructor.
newtype BatchAssociateUserStack = BatchAssociateUserStack'
  { _bausUserStackAssociations :: [UserStackAssociation]
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchAssociateUserStack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bausUserStackAssociations' - The list of UserStackAssociation objects.
batchAssociateUserStack
    :: BatchAssociateUserStack
batchAssociateUserStack =
  BatchAssociateUserStack' {_bausUserStackAssociations = mempty}


-- | The list of UserStackAssociation objects.
bausUserStackAssociations :: Lens' BatchAssociateUserStack [UserStackAssociation]
bausUserStackAssociations = lens _bausUserStackAssociations (\ s a -> s{_bausUserStackAssociations = a}) . _Coerce

instance AWSRequest BatchAssociateUserStack where
        type Rs BatchAssociateUserStack =
             BatchAssociateUserStackResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 BatchAssociateUserStackResponse' <$>
                   (x .?> "errors" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable BatchAssociateUserStack where

instance NFData BatchAssociateUserStack where

instance ToHeaders BatchAssociateUserStack where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.BatchAssociateUserStack" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchAssociateUserStack where
        toJSON BatchAssociateUserStack'{..}
          = object
              (catMaybes
                 [Just
                    ("UserStackAssociations" .=
                       _bausUserStackAssociations)])

instance ToPath BatchAssociateUserStack where
        toPath = const "/"

instance ToQuery BatchAssociateUserStack where
        toQuery = const mempty

-- | /See:/ 'batchAssociateUserStackResponse' smart constructor.
data BatchAssociateUserStackResponse = BatchAssociateUserStackResponse'
  { _bausrsErrors         :: !(Maybe [UserStackAssociationError])
  , _bausrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchAssociateUserStackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bausrsErrors' - The list of UserStackAssociationError objects.
--
-- * 'bausrsResponseStatus' - -- | The response status code.
batchAssociateUserStackResponse
    :: Int -- ^ 'bausrsResponseStatus'
    -> BatchAssociateUserStackResponse
batchAssociateUserStackResponse pResponseStatus_ =
  BatchAssociateUserStackResponse'
    {_bausrsErrors = Nothing, _bausrsResponseStatus = pResponseStatus_}


-- | The list of UserStackAssociationError objects.
bausrsErrors :: Lens' BatchAssociateUserStackResponse [UserStackAssociationError]
bausrsErrors = lens _bausrsErrors (\ s a -> s{_bausrsErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
bausrsResponseStatus :: Lens' BatchAssociateUserStackResponse Int
bausrsResponseStatus = lens _bausrsResponseStatus (\ s a -> s{_bausrsResponseStatus = a})

instance NFData BatchAssociateUserStackResponse where
