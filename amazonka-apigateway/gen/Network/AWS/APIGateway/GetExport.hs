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
-- Module      : Network.AWS.APIGateway.GetExport
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.GetExport
    (
    -- * Creating a Request
      getExport
    , GetExport
    -- * Request Lenses
    , geParameters
    , geAccepts
    , geRestAPIId
    , geStageName
    , geExportType

    -- * Destructuring the Response
    , getExportResponse
    , GetExportResponse
    -- * Response Lenses
    , gersBody
    , gersContentDisposition
    , gersContentType
    , gersResponseStatus
    ) where

import           Network.AWS.APIGateway.Types
import           Network.AWS.APIGateway.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getExport' smart constructor.
data GetExport = GetExport'
    { _geParameters :: !(Maybe (Map Text Text))
    , _geAccepts    :: !(Maybe Text)
    , _geRestAPIId  :: !Text
    , _geStageName  :: !Text
    , _geExportType :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetExport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'geParameters'
--
-- * 'geAccepts'
--
-- * 'geRestAPIId'
--
-- * 'geStageName'
--
-- * 'geExportType'
getExport
    :: Text -- ^ 'geRestAPIId'
    -> Text -- ^ 'geStageName'
    -> Text -- ^ 'geExportType'
    -> GetExport
getExport pRestAPIId_ pStageName_ pExportType_ =
    GetExport'
    { _geParameters = Nothing
    , _geAccepts = Nothing
    , _geRestAPIId = pRestAPIId_
    , _geStageName = pStageName_
    , _geExportType = pExportType_
    }

-- | Undocumented member.
geParameters :: Lens' GetExport (HashMap Text Text)
geParameters = lens _geParameters (\ s a -> s{_geParameters = a}) . _Default . _Map;

-- | Undocumented member.
geAccepts :: Lens' GetExport (Maybe Text)
geAccepts = lens _geAccepts (\ s a -> s{_geAccepts = a});

-- | Undocumented member.
geRestAPIId :: Lens' GetExport Text
geRestAPIId = lens _geRestAPIId (\ s a -> s{_geRestAPIId = a});

-- | Undocumented member.
geStageName :: Lens' GetExport Text
geStageName = lens _geStageName (\ s a -> s{_geStageName = a});

-- | Undocumented member.
geExportType :: Lens' GetExport Text
geExportType = lens _geExportType (\ s a -> s{_geExportType = a});

instance AWSRequest GetExport where
        type Rs GetExport = GetExportResponse
        request = get aPIGateway
        response
          = receiveJSON
              (\ s h x ->
                 GetExportResponse' <$>
                   (pure (Just x)) <*> (h .#? "Content-Disposition") <*>
                     (h .#? "Content-Type")
                     <*> (pure (fromEnum s)))

instance ToHeaders GetExport where
        toHeaders GetExport'{..}
          = mconcat
              ["Accept" =# _geAccepts,
               "Accept" =# ("application/json" :: ByteString)]

instance ToPath GetExport where
        toPath GetExport'{..}
          = mconcat
              ["/restapis/", toBS _geRestAPIId, "/stages/",
               toBS _geStageName, "/exports/", toBS _geExportType]

instance ToQuery GetExport where
        toQuery GetExport'{..}
          = mconcat
              ["parameters" =:
                 toQuery
                   (toQueryMap "entry" "key" "value" <$> _geParameters)]

-- | /See:/ 'getExportResponse' smart constructor.
data GetExportResponse = GetExportResponse'
    { _gersBody               :: !(Maybe (HashMap Text Value))
    , _gersContentDisposition :: !(Maybe Text)
    , _gersContentType        :: !(Maybe Text)
    , _gersResponseStatus     :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetExportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gersBody'
--
-- * 'gersContentDisposition'
--
-- * 'gersContentType'
--
-- * 'gersResponseStatus'
getExportResponse
    :: Int -- ^ 'gersResponseStatus'
    -> GetExportResponse
getExportResponse pResponseStatus_ =
    GetExportResponse'
    { _gersBody = Nothing
    , _gersContentDisposition = Nothing
    , _gersContentType = Nothing
    , _gersResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
gersBody :: Lens' GetExportResponse (Maybe (HashMap Text Value))
gersBody = lens _gersBody (\ s a -> s{_gersBody = a});

-- | Undocumented member.
gersContentDisposition :: Lens' GetExportResponse (Maybe Text)
gersContentDisposition = lens _gersContentDisposition (\ s a -> s{_gersContentDisposition = a});

-- | Undocumented member.
gersContentType :: Lens' GetExportResponse (Maybe Text)
gersContentType = lens _gersContentType (\ s a -> s{_gersContentType = a});

-- | The response status code.
gersResponseStatus :: Lens' GetExportResponse Int
gersResponseStatus = lens _gersResponseStatus (\ s a -> s{_gersResponseStatus = a});
