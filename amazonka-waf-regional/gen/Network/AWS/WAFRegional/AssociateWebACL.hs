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
-- Module      : Network.AWS.WAFRegional.AssociateWebACL
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a web ACL with a resource.
--
--
module Network.AWS.WAFRegional.AssociateWebACL
    (
    -- * Creating a Request
      associateWebACL
    , AssociateWebACL
    -- * Request Lenses
    , awaWebACLId
    , awaResourceARN

    -- * Destructuring the Response
    , associateWebACLResponse
    , AssociateWebACLResponse
    -- * Response Lenses
    , awarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'associateWebACL' smart constructor.
data AssociateWebACL = AssociateWebACL'
  { _awaWebACLId    :: !Text
  , _awaResourceARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateWebACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'awaWebACLId' - A unique identifier (ID) for the web ACL.
--
-- * 'awaResourceARN' - The ARN (Amazon Resource Name) of the resource to be protected.
associateWebACL
    :: Text -- ^ 'awaWebACLId'
    -> Text -- ^ 'awaResourceARN'
    -> AssociateWebACL
associateWebACL pWebACLId_ pResourceARN_ =
  AssociateWebACL' {_awaWebACLId = pWebACLId_, _awaResourceARN = pResourceARN_}


-- | A unique identifier (ID) for the web ACL.
awaWebACLId :: Lens' AssociateWebACL Text
awaWebACLId = lens _awaWebACLId (\ s a -> s{_awaWebACLId = a})

-- | The ARN (Amazon Resource Name) of the resource to be protected.
awaResourceARN :: Lens' AssociateWebACL Text
awaResourceARN = lens _awaResourceARN (\ s a -> s{_awaResourceARN = a})

instance AWSRequest AssociateWebACL where
        type Rs AssociateWebACL = AssociateWebACLResponse
        request = postJSON wAFRegional
        response
          = receiveEmpty
              (\ s h x ->
                 AssociateWebACLResponse' <$> (pure (fromEnum s)))

instance Hashable AssociateWebACL where

instance NFData AssociateWebACL where

instance ToHeaders AssociateWebACL where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.AssociateWebACL" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateWebACL where
        toJSON AssociateWebACL'{..}
          = object
              (catMaybes
                 [Just ("WebACLId" .= _awaWebACLId),
                  Just ("ResourceArn" .= _awaResourceARN)])

instance ToPath AssociateWebACL where
        toPath = const "/"

instance ToQuery AssociateWebACL where
        toQuery = const mempty

-- | /See:/ 'associateWebACLResponse' smart constructor.
newtype AssociateWebACLResponse = AssociateWebACLResponse'
  { _awarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateWebACLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'awarsResponseStatus' - -- | The response status code.
associateWebACLResponse
    :: Int -- ^ 'awarsResponseStatus'
    -> AssociateWebACLResponse
associateWebACLResponse pResponseStatus_ =
  AssociateWebACLResponse' {_awarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
awarsResponseStatus :: Lens' AssociateWebACLResponse Int
awarsResponseStatus = lens _awarsResponseStatus (\ s a -> s{_awarsResponseStatus = a})

instance NFData AssociateWebACLResponse where
