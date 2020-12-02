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
-- Module      : Network.AWS.LexModels.StartImport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a job to import a resource to Amazon Lex.
--
--
module Network.AWS.LexModels.StartImport
    (
    -- * Creating a Request
      startImport
    , StartImport
    -- * Request Lenses
    , siPayload
    , siResourceType
    , siMergeStrategy

    -- * Destructuring the Response
    , startImportResponse
    , StartImportResponse
    -- * Response Lenses
    , sirsResourceType
    , sirsImportId
    , sirsCreatedDate
    , sirsName
    , sirsMergeStrategy
    , sirsImportStatus
    , sirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.LexModels.Types
import Network.AWS.LexModels.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startImport' smart constructor.
data StartImport = StartImport'
  { _siPayload       :: !Base64
  , _siResourceType  :: !ResourceType
  , _siMergeStrategy :: !MergeStrategy
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartImport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siPayload' - A zip archive in binary format. The archive should contain one file, a JSON file containing the resource to import. The resource should match the type specified in the @resourceType@ field.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'siResourceType' - Specifies the type of resource to export. Each resource also exports any resources that it depends on.      * A bot exports dependent intents.     * An intent exports dependent slot types.
--
-- * 'siMergeStrategy' - Specifies the action that the @StartImport@ operation should take when there is an existing resource with the same name.     * FAIL_ON_CONFLICT - The import operation is stopped on the first conflict between a resource in the import file and an existing resource. The name of the resource causing the conflict is in the @failureReason@ field of the response to the @GetImport@ operation. OVERWRITE_LATEST - The import operation proceeds even if there is a conflict with an existing resource. The $LASTEST version of the existing resource is overwritten with the data from the import file.
startImport
    :: ByteString -- ^ 'siPayload'
    -> ResourceType -- ^ 'siResourceType'
    -> MergeStrategy -- ^ 'siMergeStrategy'
    -> StartImport
startImport pPayload_ pResourceType_ pMergeStrategy_ =
  StartImport'
    { _siPayload = _Base64 # pPayload_
    , _siResourceType = pResourceType_
    , _siMergeStrategy = pMergeStrategy_
    }


-- | A zip archive in binary format. The archive should contain one file, a JSON file containing the resource to import. The resource should match the type specified in the @resourceType@ field.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
siPayload :: Lens' StartImport ByteString
siPayload = lens _siPayload (\ s a -> s{_siPayload = a}) . _Base64

-- | Specifies the type of resource to export. Each resource also exports any resources that it depends on.      * A bot exports dependent intents.     * An intent exports dependent slot types.
siResourceType :: Lens' StartImport ResourceType
siResourceType = lens _siResourceType (\ s a -> s{_siResourceType = a})

-- | Specifies the action that the @StartImport@ operation should take when there is an existing resource with the same name.     * FAIL_ON_CONFLICT - The import operation is stopped on the first conflict between a resource in the import file and an existing resource. The name of the resource causing the conflict is in the @failureReason@ field of the response to the @GetImport@ operation. OVERWRITE_LATEST - The import operation proceeds even if there is a conflict with an existing resource. The $LASTEST version of the existing resource is overwritten with the data from the import file.
siMergeStrategy :: Lens' StartImport MergeStrategy
siMergeStrategy = lens _siMergeStrategy (\ s a -> s{_siMergeStrategy = a})

instance AWSRequest StartImport where
        type Rs StartImport = StartImportResponse
        request = postJSON lexModels
        response
          = receiveJSON
              (\ s h x ->
                 StartImportResponse' <$>
                   (x .?> "resourceType") <*> (x .?> "importId") <*>
                     (x .?> "createdDate")
                     <*> (x .?> "name")
                     <*> (x .?> "mergeStrategy")
                     <*> (x .?> "importStatus")
                     <*> (pure (fromEnum s)))

instance Hashable StartImport where

instance NFData StartImport where

instance ToHeaders StartImport where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartImport where
        toJSON StartImport'{..}
          = object
              (catMaybes
                 [Just ("payload" .= _siPayload),
                  Just ("resourceType" .= _siResourceType),
                  Just ("mergeStrategy" .= _siMergeStrategy)])

instance ToPath StartImport where
        toPath = const "/imports/"

instance ToQuery StartImport where
        toQuery = const mempty

-- | /See:/ 'startImportResponse' smart constructor.
data StartImportResponse = StartImportResponse'
  { _sirsResourceType   :: !(Maybe ResourceType)
  , _sirsImportId       :: !(Maybe Text)
  , _sirsCreatedDate    :: !(Maybe POSIX)
  , _sirsName           :: !(Maybe Text)
  , _sirsMergeStrategy  :: !(Maybe MergeStrategy)
  , _sirsImportStatus   :: !(Maybe ImportStatus)
  , _sirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartImportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sirsResourceType' - The type of resource to import.
--
-- * 'sirsImportId' - The identifier for the specific import job.
--
-- * 'sirsCreatedDate' - A timestamp for the date and time that the import job was requested.
--
-- * 'sirsName' - The name given to the import job.
--
-- * 'sirsMergeStrategy' - The action to take when there is a merge conflict.
--
-- * 'sirsImportStatus' - The status of the import job. If the status is @FAILED@ , you can get the reason for the failure using the @GetImport@ operation.
--
-- * 'sirsResponseStatus' - -- | The response status code.
startImportResponse
    :: Int -- ^ 'sirsResponseStatus'
    -> StartImportResponse
startImportResponse pResponseStatus_ =
  StartImportResponse'
    { _sirsResourceType = Nothing
    , _sirsImportId = Nothing
    , _sirsCreatedDate = Nothing
    , _sirsName = Nothing
    , _sirsMergeStrategy = Nothing
    , _sirsImportStatus = Nothing
    , _sirsResponseStatus = pResponseStatus_
    }


-- | The type of resource to import.
sirsResourceType :: Lens' StartImportResponse (Maybe ResourceType)
sirsResourceType = lens _sirsResourceType (\ s a -> s{_sirsResourceType = a})

-- | The identifier for the specific import job.
sirsImportId :: Lens' StartImportResponse (Maybe Text)
sirsImportId = lens _sirsImportId (\ s a -> s{_sirsImportId = a})

-- | A timestamp for the date and time that the import job was requested.
sirsCreatedDate :: Lens' StartImportResponse (Maybe UTCTime)
sirsCreatedDate = lens _sirsCreatedDate (\ s a -> s{_sirsCreatedDate = a}) . mapping _Time

-- | The name given to the import job.
sirsName :: Lens' StartImportResponse (Maybe Text)
sirsName = lens _sirsName (\ s a -> s{_sirsName = a})

-- | The action to take when there is a merge conflict.
sirsMergeStrategy :: Lens' StartImportResponse (Maybe MergeStrategy)
sirsMergeStrategy = lens _sirsMergeStrategy (\ s a -> s{_sirsMergeStrategy = a})

-- | The status of the import job. If the status is @FAILED@ , you can get the reason for the failure using the @GetImport@ operation.
sirsImportStatus :: Lens' StartImportResponse (Maybe ImportStatus)
sirsImportStatus = lens _sirsImportStatus (\ s a -> s{_sirsImportStatus = a})

-- | -- | The response status code.
sirsResponseStatus :: Lens' StartImportResponse Int
sirsResponseStatus = lens _sirsResponseStatus (\ s a -> s{_sirsResponseStatus = a})

instance NFData StartImportResponse where
