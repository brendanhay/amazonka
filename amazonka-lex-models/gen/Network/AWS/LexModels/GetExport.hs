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
-- Module      : Network.AWS.LexModels.GetExport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports the contents of a Amazon Lex resource in a specified format.
--
--
module Network.AWS.LexModels.GetExport
    (
    -- * Creating a Request
      getExport
    , GetExport
    -- * Request Lenses
    , geName
    , geVersion
    , geResourceType
    , geExportType

    -- * Destructuring the Response
    , getExportResponse
    , GetExportResponse
    -- * Response Lenses
    , gersFailureReason
    , gersResourceType
    , gersExportStatus
    , gersUrl
    , gersExportType
    , gersName
    , gersVersion
    , gersResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getExport' smart constructor.
data GetExport = GetExport'
  { _geName         :: !Text
  , _geVersion      :: !Text
  , _geResourceType :: !ResourceType
  , _geExportType   :: !ExportType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetExport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'geName' - The name of the bot to export.
--
-- * 'geVersion' - The version of the bot to export.
--
-- * 'geResourceType' - The type of resource to export.
--
-- * 'geExportType' - The format of the exported data.
getExport
    :: Text -- ^ 'geName'
    -> Text -- ^ 'geVersion'
    -> ResourceType -- ^ 'geResourceType'
    -> ExportType -- ^ 'geExportType'
    -> GetExport
getExport pName_ pVersion_ pResourceType_ pExportType_ =
  GetExport'
    { _geName = pName_
    , _geVersion = pVersion_
    , _geResourceType = pResourceType_
    , _geExportType = pExportType_
    }


-- | The name of the bot to export.
geName :: Lens' GetExport Text
geName = lens _geName (\ s a -> s{_geName = a})

-- | The version of the bot to export.
geVersion :: Lens' GetExport Text
geVersion = lens _geVersion (\ s a -> s{_geVersion = a})

-- | The type of resource to export.
geResourceType :: Lens' GetExport ResourceType
geResourceType = lens _geResourceType (\ s a -> s{_geResourceType = a})

-- | The format of the exported data.
geExportType :: Lens' GetExport ExportType
geExportType = lens _geExportType (\ s a -> s{_geExportType = a})

instance AWSRequest GetExport where
        type Rs GetExport = GetExportResponse
        request = get lexModels
        response
          = receiveJSON
              (\ s h x ->
                 GetExportResponse' <$>
                   (x .?> "failureReason") <*> (x .?> "resourceType")
                     <*> (x .?> "exportStatus")
                     <*> (x .?> "url")
                     <*> (x .?> "exportType")
                     <*> (x .?> "name")
                     <*> (x .?> "version")
                     <*> (pure (fromEnum s)))

instance Hashable GetExport where

instance NFData GetExport where

instance ToHeaders GetExport where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetExport where
        toPath = const "/exports/"

instance ToQuery GetExport where
        toQuery GetExport'{..}
          = mconcat
              ["name" =: _geName, "version" =: _geVersion,
               "resourceType" =: _geResourceType,
               "exportType" =: _geExportType]

-- | /See:/ 'getExportResponse' smart constructor.
data GetExportResponse = GetExportResponse'
  { _gersFailureReason  :: !(Maybe Text)
  , _gersResourceType   :: !(Maybe ResourceType)
  , _gersExportStatus   :: !(Maybe ExportStatus)
  , _gersUrl            :: !(Maybe Text)
  , _gersExportType     :: !(Maybe ExportType)
  , _gersName           :: !(Maybe Text)
  , _gersVersion        :: !(Maybe Text)
  , _gersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetExportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gersFailureReason' - If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to export the resource.
--
-- * 'gersResourceType' - The type of the exported resource.
--
-- * 'gersExportStatus' - The status of the export.      * @IN_PROGRESS@ - The export is in progress.     * @READY@ - The export is complete.     * @FAILED@ - The export could not be completed.
--
-- * 'gersUrl' - An S3 pre-signed URL that provides the location of the exported resource. The exported resource is a ZIP archive that contains the exported resource in JSON format. The structure of the archive may change. Your code should not rely on the archive structure.
--
-- * 'gersExportType' - The format of the exported data.
--
-- * 'gersName' - The name of the bot being exported.
--
-- * 'gersVersion' - The version of the bot being exported.
--
-- * 'gersResponseStatus' - -- | The response status code.
getExportResponse
    :: Int -- ^ 'gersResponseStatus'
    -> GetExportResponse
getExportResponse pResponseStatus_ =
  GetExportResponse'
    { _gersFailureReason = Nothing
    , _gersResourceType = Nothing
    , _gersExportStatus = Nothing
    , _gersUrl = Nothing
    , _gersExportType = Nothing
    , _gersName = Nothing
    , _gersVersion = Nothing
    , _gersResponseStatus = pResponseStatus_
    }


-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to export the resource.
gersFailureReason :: Lens' GetExportResponse (Maybe Text)
gersFailureReason = lens _gersFailureReason (\ s a -> s{_gersFailureReason = a})

-- | The type of the exported resource.
gersResourceType :: Lens' GetExportResponse (Maybe ResourceType)
gersResourceType = lens _gersResourceType (\ s a -> s{_gersResourceType = a})

-- | The status of the export.      * @IN_PROGRESS@ - The export is in progress.     * @READY@ - The export is complete.     * @FAILED@ - The export could not be completed.
gersExportStatus :: Lens' GetExportResponse (Maybe ExportStatus)
gersExportStatus = lens _gersExportStatus (\ s a -> s{_gersExportStatus = a})

-- | An S3 pre-signed URL that provides the location of the exported resource. The exported resource is a ZIP archive that contains the exported resource in JSON format. The structure of the archive may change. Your code should not rely on the archive structure.
gersUrl :: Lens' GetExportResponse (Maybe Text)
gersUrl = lens _gersUrl (\ s a -> s{_gersUrl = a})

-- | The format of the exported data.
gersExportType :: Lens' GetExportResponse (Maybe ExportType)
gersExportType = lens _gersExportType (\ s a -> s{_gersExportType = a})

-- | The name of the bot being exported.
gersName :: Lens' GetExportResponse (Maybe Text)
gersName = lens _gersName (\ s a -> s{_gersName = a})

-- | The version of the bot being exported.
gersVersion :: Lens' GetExportResponse (Maybe Text)
gersVersion = lens _gersVersion (\ s a -> s{_gersVersion = a})

-- | -- | The response status code.
gersResponseStatus :: Lens' GetExportResponse Int
gersResponseStatus = lens _gersResponseStatus (\ s a -> s{_gersResponseStatus = a})

instance NFData GetExportResponse where
