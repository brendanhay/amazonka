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
-- Module      : Network.AWS.Discovery.DisassociateConfigurationItemsFromApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates one or more configuration items from an application.
--
--
module Network.AWS.Discovery.DisassociateConfigurationItemsFromApplication
    (
    -- * Creating a Request
      disassociateConfigurationItemsFromApplication
    , DisassociateConfigurationItemsFromApplication
    -- * Request Lenses
    , dcifaApplicationConfigurationId
    , dcifaConfigurationIds

    -- * Destructuring the Response
    , disassociateConfigurationItemsFromApplicationResponse
    , DisassociateConfigurationItemsFromApplicationResponse
    -- * Response Lenses
    , dcifarsResponseStatus
    ) where

import Network.AWS.Discovery.Types
import Network.AWS.Discovery.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateConfigurationItemsFromApplication' smart constructor.
data DisassociateConfigurationItemsFromApplication = DisassociateConfigurationItemsFromApplication'
  { _dcifaApplicationConfigurationId :: !Text
  , _dcifaConfigurationIds           :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateConfigurationItemsFromApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcifaApplicationConfigurationId' - Configuration ID of an application from which each item is disassociated.
--
-- * 'dcifaConfigurationIds' - Configuration ID of each item to be disassociated from an application.
disassociateConfigurationItemsFromApplication
    :: Text -- ^ 'dcifaApplicationConfigurationId'
    -> DisassociateConfigurationItemsFromApplication
disassociateConfigurationItemsFromApplication pApplicationConfigurationId_ =
  DisassociateConfigurationItemsFromApplication'
    { _dcifaApplicationConfigurationId = pApplicationConfigurationId_
    , _dcifaConfigurationIds = mempty
    }


-- | Configuration ID of an application from which each item is disassociated.
dcifaApplicationConfigurationId :: Lens' DisassociateConfigurationItemsFromApplication Text
dcifaApplicationConfigurationId = lens _dcifaApplicationConfigurationId (\ s a -> s{_dcifaApplicationConfigurationId = a})

-- | Configuration ID of each item to be disassociated from an application.
dcifaConfigurationIds :: Lens' DisassociateConfigurationItemsFromApplication [Text]
dcifaConfigurationIds = lens _dcifaConfigurationIds (\ s a -> s{_dcifaConfigurationIds = a}) . _Coerce

instance AWSRequest
           DisassociateConfigurationItemsFromApplication
         where
        type Rs DisassociateConfigurationItemsFromApplication
             =
             DisassociateConfigurationItemsFromApplicationResponse
        request = postJSON discovery
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateConfigurationItemsFromApplicationResponse'
                   <$> (pure (fromEnum s)))

instance Hashable
           DisassociateConfigurationItemsFromApplication
         where

instance NFData
           DisassociateConfigurationItemsFromApplication
         where

instance ToHeaders
           DisassociateConfigurationItemsFromApplication
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.DisassociateConfigurationItemsFromApplication"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           DisassociateConfigurationItemsFromApplication
         where
        toJSON
          DisassociateConfigurationItemsFromApplication'{..}
          = object
              (catMaybes
                 [Just
                    ("applicationConfigurationId" .=
                       _dcifaApplicationConfigurationId),
                  Just ("configurationIds" .= _dcifaConfigurationIds)])

instance ToPath
           DisassociateConfigurationItemsFromApplication
         where
        toPath = const "/"

instance ToQuery
           DisassociateConfigurationItemsFromApplication
         where
        toQuery = const mempty

-- | /See:/ 'disassociateConfigurationItemsFromApplicationResponse' smart constructor.
newtype DisassociateConfigurationItemsFromApplicationResponse = DisassociateConfigurationItemsFromApplicationResponse'
  { _dcifarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateConfigurationItemsFromApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcifarsResponseStatus' - -- | The response status code.
disassociateConfigurationItemsFromApplicationResponse
    :: Int -- ^ 'dcifarsResponseStatus'
    -> DisassociateConfigurationItemsFromApplicationResponse
disassociateConfigurationItemsFromApplicationResponse pResponseStatus_ =
  DisassociateConfigurationItemsFromApplicationResponse'
    {_dcifarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dcifarsResponseStatus :: Lens' DisassociateConfigurationItemsFromApplicationResponse Int
dcifarsResponseStatus = lens _dcifarsResponseStatus (\ s a -> s{_dcifarsResponseStatus = a})

instance NFData
           DisassociateConfigurationItemsFromApplicationResponse
         where
