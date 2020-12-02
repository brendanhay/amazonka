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
-- Module      : Network.AWS.LexModels.GetImport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an import job started with the @StartImport@ operation.
--
--
module Network.AWS.LexModels.GetImport
    (
    -- * Creating a Request
      getImport
    , GetImport
    -- * Request Lenses
    , giImportId

    -- * Destructuring the Response
    , getImportResponse
    , GetImportResponse
    -- * Response Lenses
    , girsFailureReason
    , girsResourceType
    , girsImportId
    , girsCreatedDate
    , girsName
    , girsMergeStrategy
    , girsImportStatus
    , girsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getImport' smart constructor.
newtype GetImport = GetImport'
  { _giImportId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetImport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giImportId' - The identifier of the import job information to return.
getImport
    :: Text -- ^ 'giImportId'
    -> GetImport
getImport pImportId_ = GetImport' {_giImportId = pImportId_}


-- | The identifier of the import job information to return.
giImportId :: Lens' GetImport Text
giImportId = lens _giImportId (\ s a -> s{_giImportId = a})

instance AWSRequest GetImport where
        type Rs GetImport = GetImportResponse
        request = get lexModels
        response
          = receiveJSON
              (\ s h x ->
                 GetImportResponse' <$>
                   (x .?> "failureReason" .!@ mempty) <*>
                     (x .?> "resourceType")
                     <*> (x .?> "importId")
                     <*> (x .?> "createdDate")
                     <*> (x .?> "name")
                     <*> (x .?> "mergeStrategy")
                     <*> (x .?> "importStatus")
                     <*> (pure (fromEnum s)))

instance Hashable GetImport where

instance NFData GetImport where

instance ToHeaders GetImport where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetImport where
        toPath GetImport'{..}
          = mconcat ["/imports/", toBS _giImportId]

instance ToQuery GetImport where
        toQuery = const mempty

-- | /See:/ 'getImportResponse' smart constructor.
data GetImportResponse = GetImportResponse'
  { _girsFailureReason  :: !(Maybe [Text])
  , _girsResourceType   :: !(Maybe ResourceType)
  , _girsImportId       :: !(Maybe Text)
  , _girsCreatedDate    :: !(Maybe POSIX)
  , _girsName           :: !(Maybe Text)
  , _girsMergeStrategy  :: !(Maybe MergeStrategy)
  , _girsImportStatus   :: !(Maybe ImportStatus)
  , _girsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetImportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'girsFailureReason' - A string that describes why an import job failed to complete.
--
-- * 'girsResourceType' - The type of resource imported.
--
-- * 'girsImportId' - The identifier for the specific import job.
--
-- * 'girsCreatedDate' - A timestamp for the date and time that the import job was created.
--
-- * 'girsName' - The name given to the import job.
--
-- * 'girsMergeStrategy' - The action taken when there was a conflict between an existing resource and a resource in the import file.
--
-- * 'girsImportStatus' - The status of the import job. If the status is @FAILED@ , you can get the reason for the failure from the @failureReason@ field.
--
-- * 'girsResponseStatus' - -- | The response status code.
getImportResponse
    :: Int -- ^ 'girsResponseStatus'
    -> GetImportResponse
getImportResponse pResponseStatus_ =
  GetImportResponse'
    { _girsFailureReason = Nothing
    , _girsResourceType = Nothing
    , _girsImportId = Nothing
    , _girsCreatedDate = Nothing
    , _girsName = Nothing
    , _girsMergeStrategy = Nothing
    , _girsImportStatus = Nothing
    , _girsResponseStatus = pResponseStatus_
    }


-- | A string that describes why an import job failed to complete.
girsFailureReason :: Lens' GetImportResponse [Text]
girsFailureReason = lens _girsFailureReason (\ s a -> s{_girsFailureReason = a}) . _Default . _Coerce

-- | The type of resource imported.
girsResourceType :: Lens' GetImportResponse (Maybe ResourceType)
girsResourceType = lens _girsResourceType (\ s a -> s{_girsResourceType = a})

-- | The identifier for the specific import job.
girsImportId :: Lens' GetImportResponse (Maybe Text)
girsImportId = lens _girsImportId (\ s a -> s{_girsImportId = a})

-- | A timestamp for the date and time that the import job was created.
girsCreatedDate :: Lens' GetImportResponse (Maybe UTCTime)
girsCreatedDate = lens _girsCreatedDate (\ s a -> s{_girsCreatedDate = a}) . mapping _Time

-- | The name given to the import job.
girsName :: Lens' GetImportResponse (Maybe Text)
girsName = lens _girsName (\ s a -> s{_girsName = a})

-- | The action taken when there was a conflict between an existing resource and a resource in the import file.
girsMergeStrategy :: Lens' GetImportResponse (Maybe MergeStrategy)
girsMergeStrategy = lens _girsMergeStrategy (\ s a -> s{_girsMergeStrategy = a})

-- | The status of the import job. If the status is @FAILED@ , you can get the reason for the failure from the @failureReason@ field.
girsImportStatus :: Lens' GetImportResponse (Maybe ImportStatus)
girsImportStatus = lens _girsImportStatus (\ s a -> s{_girsImportStatus = a})

-- | -- | The response status code.
girsResponseStatus :: Lens' GetImportResponse Int
girsResponseStatus = lens _girsResponseStatus (\ s a -> s{_girsResponseStatus = a})

instance NFData GetImportResponse where
