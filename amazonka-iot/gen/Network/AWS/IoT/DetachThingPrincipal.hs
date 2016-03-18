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
-- Module      : Network.AWS.IoT.DetachThingPrincipal
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches the specified principal from the specified thing.
module Network.AWS.IoT.DetachThingPrincipal
    (
    -- * Creating a Request
      detachThingPrincipal
    , DetachThingPrincipal
    -- * Request Lenses
    , dtpThingName
    , dtpPrincipal

    -- * Destructuring the Response
    , detachThingPrincipalResponse
    , DetachThingPrincipalResponse
    -- * Response Lenses
    , dtprsResponseStatus
    ) where

import           Network.AWS.IoT.Types
import           Network.AWS.IoT.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the DetachThingPrincipal operation.
--
-- /See:/ 'detachThingPrincipal' smart constructor.
data DetachThingPrincipal = DetachThingPrincipal'
    { _dtpThingName :: !Text
    , _dtpPrincipal :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DetachThingPrincipal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtpThingName'
--
-- * 'dtpPrincipal'
detachThingPrincipal
    :: Text -- ^ 'dtpThingName'
    -> Text -- ^ 'dtpPrincipal'
    -> DetachThingPrincipal
detachThingPrincipal pThingName_ pPrincipal_ =
    DetachThingPrincipal'
    { _dtpThingName = pThingName_
    , _dtpPrincipal = pPrincipal_
    }

-- | The name of the thing.
dtpThingName :: Lens' DetachThingPrincipal Text
dtpThingName = lens _dtpThingName (\ s a -> s{_dtpThingName = a});

-- | The principal.
dtpPrincipal :: Lens' DetachThingPrincipal Text
dtpPrincipal = lens _dtpPrincipal (\ s a -> s{_dtpPrincipal = a});

instance AWSRequest DetachThingPrincipal where
        type Rs DetachThingPrincipal =
             DetachThingPrincipalResponse
        request = delete ioT
        response
          = receiveEmpty
              (\ s h x ->
                 DetachThingPrincipalResponse' <$>
                   (pure (fromEnum s)))

instance ToHeaders DetachThingPrincipal where
        toHeaders DetachThingPrincipal'{..}
          = mconcat ["x-amzn-principal" =# _dtpPrincipal]

instance ToPath DetachThingPrincipal where
        toPath DetachThingPrincipal'{..}
          = mconcat
              ["/things/", toBS _dtpThingName, "/principals"]

instance ToQuery DetachThingPrincipal where
        toQuery = const mempty

-- | The output from the DetachThingPrincipal operation.
--
-- /See:/ 'detachThingPrincipalResponse' smart constructor.
newtype DetachThingPrincipalResponse = DetachThingPrincipalResponse'
    { _dtprsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DetachThingPrincipalResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtprsResponseStatus'
detachThingPrincipalResponse
    :: Int -- ^ 'dtprsResponseStatus'
    -> DetachThingPrincipalResponse
detachThingPrincipalResponse pResponseStatus_ =
    DetachThingPrincipalResponse'
    { _dtprsResponseStatus = pResponseStatus_
    }

-- | The response status code.
dtprsResponseStatus :: Lens' DetachThingPrincipalResponse Int
dtprsResponseStatus = lens _dtprsResponseStatus (\ s a -> s{_dtprsResponseStatus = a});
