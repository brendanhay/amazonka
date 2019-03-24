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
-- Module      : Network.AWS.AppStream.BatchDisassociateUserStack
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified users from the specified stacks.
--
--
module Network.AWS.AppStream.BatchDisassociateUserStack
    (
    -- * Creating a Request
      batchDisassociateUserStack
    , BatchDisassociateUserStack
    -- * Request Lenses
    , bdusUserStackAssociations

    -- * Destructuring the Response
    , batchDisassociateUserStackResponse
    , BatchDisassociateUserStackResponse
    -- * Response Lenses
    , bdusrsErrors
    , bdusrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchDisassociateUserStack' smart constructor.
newtype BatchDisassociateUserStack = BatchDisassociateUserStack'
  { _bdusUserStackAssociations :: [UserStackAssociation]
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDisassociateUserStack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdusUserStackAssociations' - The list of UserStackAssociation objects.
batchDisassociateUserStack
    :: BatchDisassociateUserStack
batchDisassociateUserStack =
  BatchDisassociateUserStack' {_bdusUserStackAssociations = mempty}


-- | The list of UserStackAssociation objects.
bdusUserStackAssociations :: Lens' BatchDisassociateUserStack [UserStackAssociation]
bdusUserStackAssociations = lens _bdusUserStackAssociations (\ s a -> s{_bdusUserStackAssociations = a}) . _Coerce

instance AWSRequest BatchDisassociateUserStack where
        type Rs BatchDisassociateUserStack =
             BatchDisassociateUserStackResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 BatchDisassociateUserStackResponse' <$>
                   (x .?> "errors" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable BatchDisassociateUserStack where

instance NFData BatchDisassociateUserStack where

instance ToHeaders BatchDisassociateUserStack where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.BatchDisassociateUserStack"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchDisassociateUserStack where
        toJSON BatchDisassociateUserStack'{..}
          = object
              (catMaybes
                 [Just
                    ("UserStackAssociations" .=
                       _bdusUserStackAssociations)])

instance ToPath BatchDisassociateUserStack where
        toPath = const "/"

instance ToQuery BatchDisassociateUserStack where
        toQuery = const mempty

-- | /See:/ 'batchDisassociateUserStackResponse' smart constructor.
data BatchDisassociateUserStackResponse = BatchDisassociateUserStackResponse'
  { _bdusrsErrors         :: !(Maybe [UserStackAssociationError])
  , _bdusrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDisassociateUserStackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdusrsErrors' - The list of UserStackAssociationError objects.
--
-- * 'bdusrsResponseStatus' - -- | The response status code.
batchDisassociateUserStackResponse
    :: Int -- ^ 'bdusrsResponseStatus'
    -> BatchDisassociateUserStackResponse
batchDisassociateUserStackResponse pResponseStatus_ =
  BatchDisassociateUserStackResponse'
    {_bdusrsErrors = Nothing, _bdusrsResponseStatus = pResponseStatus_}


-- | The list of UserStackAssociationError objects.
bdusrsErrors :: Lens' BatchDisassociateUserStackResponse [UserStackAssociationError]
bdusrsErrors = lens _bdusrsErrors (\ s a -> s{_bdusrsErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
bdusrsResponseStatus :: Lens' BatchDisassociateUserStackResponse Int
bdusrsResponseStatus = lens _bdusrsResponseStatus (\ s a -> s{_bdusrsResponseStatus = a})

instance NFData BatchDisassociateUserStackResponse
         where
