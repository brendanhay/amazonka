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
-- Module      : Network.AWS.IoT.AttachThingPrincipal
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified principal to the specified thing.
--
--
module Network.AWS.IoT.AttachThingPrincipal
    (
    -- * Creating a Request
      attachThingPrincipal
    , AttachThingPrincipal
    -- * Request Lenses
    , atpThingName
    , atpPrincipal

    -- * Destructuring the Response
    , attachThingPrincipalResponse
    , AttachThingPrincipalResponse
    -- * Response Lenses
    , atprsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the AttachThingPrincipal operation.
--
--
--
-- /See:/ 'attachThingPrincipal' smart constructor.
data AttachThingPrincipal = AttachThingPrincipal'
  { _atpThingName :: !Text
  , _atpPrincipal :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachThingPrincipal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atpThingName' - The name of the thing.
--
-- * 'atpPrincipal' - The principal, such as a certificate or other credential.
attachThingPrincipal
    :: Text -- ^ 'atpThingName'
    -> Text -- ^ 'atpPrincipal'
    -> AttachThingPrincipal
attachThingPrincipal pThingName_ pPrincipal_ =
  AttachThingPrincipal'
    {_atpThingName = pThingName_, _atpPrincipal = pPrincipal_}


-- | The name of the thing.
atpThingName :: Lens' AttachThingPrincipal Text
atpThingName = lens _atpThingName (\ s a -> s{_atpThingName = a})

-- | The principal, such as a certificate or other credential.
atpPrincipal :: Lens' AttachThingPrincipal Text
atpPrincipal = lens _atpPrincipal (\ s a -> s{_atpPrincipal = a})

instance AWSRequest AttachThingPrincipal where
        type Rs AttachThingPrincipal =
             AttachThingPrincipalResponse
        request = putJSON ioT
        response
          = receiveEmpty
              (\ s h x ->
                 AttachThingPrincipalResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AttachThingPrincipal where

instance NFData AttachThingPrincipal where

instance ToHeaders AttachThingPrincipal where
        toHeaders AttachThingPrincipal'{..}
          = mconcat ["x-amzn-principal" =# _atpPrincipal]

instance ToJSON AttachThingPrincipal where
        toJSON = const (Object mempty)

instance ToPath AttachThingPrincipal where
        toPath AttachThingPrincipal'{..}
          = mconcat
              ["/things/", toBS _atpThingName, "/principals"]

instance ToQuery AttachThingPrincipal where
        toQuery = const mempty

-- | The output from the AttachThingPrincipal operation.
--
--
--
-- /See:/ 'attachThingPrincipalResponse' smart constructor.
newtype AttachThingPrincipalResponse = AttachThingPrincipalResponse'
  { _atprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachThingPrincipalResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atprsResponseStatus' - -- | The response status code.
attachThingPrincipalResponse
    :: Int -- ^ 'atprsResponseStatus'
    -> AttachThingPrincipalResponse
attachThingPrincipalResponse pResponseStatus_ =
  AttachThingPrincipalResponse' {_atprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
atprsResponseStatus :: Lens' AttachThingPrincipalResponse Int
atprsResponseStatus = lens _atprsResponseStatus (\ s a -> s{_atprsResponseStatus = a})

instance NFData AttachThingPrincipalResponse where
