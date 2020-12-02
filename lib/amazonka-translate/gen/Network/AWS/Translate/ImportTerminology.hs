{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.ImportTerminology
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a custom terminology, depending on whether or not one already exists for the given terminology name. Importing a terminology with the same name as an existing one will merge the terminologies based on the chosen merge strategy. Currently, the only supported merge strategy is OVERWRITE, and so the imported terminology will overwrite an existing terminology of the same name.
--
--
-- If you import a terminology that overwrites an existing one, the new terminology take up to 10 minutes to fully propagate and be available for use in a translation due to cache policies with the DataPlane service that performs the translations.
module Network.AWS.Translate.ImportTerminology
  ( -- * Creating a Request
    importTerminology,
    ImportTerminology,

    -- * Request Lenses
    itEncryptionKey,
    itDescription,
    itName,
    itMergeStrategy,
    itTerminologyData,

    -- * Destructuring the Response
    importTerminologyResponse,
    ImportTerminologyResponse,

    -- * Response Lenses
    itrsTerminologyProperties,
    itrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Translate.Types

-- | /See:/ 'importTerminology' smart constructor.
data ImportTerminology = ImportTerminology'
  { _itEncryptionKey ::
      !(Maybe EncryptionKey),
    _itDescription :: !(Maybe Text),
    _itName :: !Text,
    _itMergeStrategy :: !MergeStrategy,
    _itTerminologyData :: !TerminologyData
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportTerminology' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itEncryptionKey' - The encryption key for the custom terminology being imported.
--
-- * 'itDescription' - The description of the custom terminology being imported.
--
-- * 'itName' - The name of the custom terminology being imported.
--
-- * 'itMergeStrategy' - The merge strategy of the custom terminology being imported. Currently, only the OVERWRITE merge strategy is supported. In this case, the imported terminology will overwrite an existing terminology of the same name.
--
-- * 'itTerminologyData' - The terminology data for the custom terminology being imported.
importTerminology ::
  -- | 'itName'
  Text ->
  -- | 'itMergeStrategy'
  MergeStrategy ->
  -- | 'itTerminologyData'
  TerminologyData ->
  ImportTerminology
importTerminology pName_ pMergeStrategy_ pTerminologyData_ =
  ImportTerminology'
    { _itEncryptionKey = Nothing,
      _itDescription = Nothing,
      _itName = pName_,
      _itMergeStrategy = pMergeStrategy_,
      _itTerminologyData = pTerminologyData_
    }

-- | The encryption key for the custom terminology being imported.
itEncryptionKey :: Lens' ImportTerminology (Maybe EncryptionKey)
itEncryptionKey = lens _itEncryptionKey (\s a -> s {_itEncryptionKey = a})

-- | The description of the custom terminology being imported.
itDescription :: Lens' ImportTerminology (Maybe Text)
itDescription = lens _itDescription (\s a -> s {_itDescription = a})

-- | The name of the custom terminology being imported.
itName :: Lens' ImportTerminology Text
itName = lens _itName (\s a -> s {_itName = a})

-- | The merge strategy of the custom terminology being imported. Currently, only the OVERWRITE merge strategy is supported. In this case, the imported terminology will overwrite an existing terminology of the same name.
itMergeStrategy :: Lens' ImportTerminology MergeStrategy
itMergeStrategy = lens _itMergeStrategy (\s a -> s {_itMergeStrategy = a})

-- | The terminology data for the custom terminology being imported.
itTerminologyData :: Lens' ImportTerminology TerminologyData
itTerminologyData = lens _itTerminologyData (\s a -> s {_itTerminologyData = a})

instance AWSRequest ImportTerminology where
  type Rs ImportTerminology = ImportTerminologyResponse
  request = postJSON translate
  response =
    receiveJSON
      ( \s h x ->
          ImportTerminologyResponse'
            <$> (x .?> "TerminologyProperties") <*> (pure (fromEnum s))
      )

instance Hashable ImportTerminology

instance NFData ImportTerminology

instance ToHeaders ImportTerminology where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSShineFrontendService_20170701.ImportTerminology" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ImportTerminology where
  toJSON ImportTerminology' {..} =
    object
      ( catMaybes
          [ ("EncryptionKey" .=) <$> _itEncryptionKey,
            ("Description" .=) <$> _itDescription,
            Just ("Name" .= _itName),
            Just ("MergeStrategy" .= _itMergeStrategy),
            Just ("TerminologyData" .= _itTerminologyData)
          ]
      )

instance ToPath ImportTerminology where
  toPath = const "/"

instance ToQuery ImportTerminology where
  toQuery = const mempty

-- | /See:/ 'importTerminologyResponse' smart constructor.
data ImportTerminologyResponse = ImportTerminologyResponse'
  { _itrsTerminologyProperties ::
      !(Maybe TerminologyProperties),
    _itrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportTerminologyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itrsTerminologyProperties' - The properties of the custom terminology being imported.
--
-- * 'itrsResponseStatus' - -- | The response status code.
importTerminologyResponse ::
  -- | 'itrsResponseStatus'
  Int ->
  ImportTerminologyResponse
importTerminologyResponse pResponseStatus_ =
  ImportTerminologyResponse'
    { _itrsTerminologyProperties = Nothing,
      _itrsResponseStatus = pResponseStatus_
    }

-- | The properties of the custom terminology being imported.
itrsTerminologyProperties :: Lens' ImportTerminologyResponse (Maybe TerminologyProperties)
itrsTerminologyProperties = lens _itrsTerminologyProperties (\s a -> s {_itrsTerminologyProperties = a})

-- | -- | The response status code.
itrsResponseStatus :: Lens' ImportTerminologyResponse Int
itrsResponseStatus = lens _itrsResponseStatus (\s a -> s {_itrsResponseStatus = a})

instance NFData ImportTerminologyResponse
