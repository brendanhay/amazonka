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
-- Module      : Network.AWS.WAFRegional.DisassociateWebACL
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a web ACL from the specified resource.
--
--
module Network.AWS.WAFRegional.DisassociateWebACL
    (
    -- * Creating a Request
      disassociateWebACL
    , DisassociateWebACL
    -- * Request Lenses
    , dwaResourceARN

    -- * Destructuring the Response
    , disassociateWebACLResponse
    , DisassociateWebACLResponse
    -- * Response Lenses
    , dwaclrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'disassociateWebACL' smart constructor.
newtype DisassociateWebACL = DisassociateWebACL'
  { _dwaResourceARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateWebACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwaResourceARN' - The ARN (Amazon Resource Name) of the resource from which the web ACL is being removed.
disassociateWebACL
    :: Text -- ^ 'dwaResourceARN'
    -> DisassociateWebACL
disassociateWebACL pResourceARN_ =
  DisassociateWebACL' {_dwaResourceARN = pResourceARN_}


-- | The ARN (Amazon Resource Name) of the resource from which the web ACL is being removed.
dwaResourceARN :: Lens' DisassociateWebACL Text
dwaResourceARN = lens _dwaResourceARN (\ s a -> s{_dwaResourceARN = a})

instance AWSRequest DisassociateWebACL where
        type Rs DisassociateWebACL =
             DisassociateWebACLResponse
        request = postJSON wAFRegional
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateWebACLResponse' <$> (pure (fromEnum s)))

instance Hashable DisassociateWebACL where

instance NFData DisassociateWebACL where

instance ToHeaders DisassociateWebACL where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.DisassociateWebACL" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateWebACL where
        toJSON DisassociateWebACL'{..}
          = object
              (catMaybes [Just ("ResourceArn" .= _dwaResourceARN)])

instance ToPath DisassociateWebACL where
        toPath = const "/"

instance ToQuery DisassociateWebACL where
        toQuery = const mempty

-- | /See:/ 'disassociateWebACLResponse' smart constructor.
newtype DisassociateWebACLResponse = DisassociateWebACLResponse'
  { _dwaclrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateWebACLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwaclrsResponseStatus' - -- | The response status code.
disassociateWebACLResponse
    :: Int -- ^ 'dwaclrsResponseStatus'
    -> DisassociateWebACLResponse
disassociateWebACLResponse pResponseStatus_ =
  DisassociateWebACLResponse' {_dwaclrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dwaclrsResponseStatus :: Lens' DisassociateWebACLResponse Int
dwaclrsResponseStatus = lens _dwaclrsResponseStatus (\ s a -> s{_dwaclrsResponseStatus = a})

instance NFData DisassociateWebACLResponse where
