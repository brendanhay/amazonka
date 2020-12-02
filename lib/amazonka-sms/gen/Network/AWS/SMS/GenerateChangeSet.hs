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
-- Module      : Network.AWS.SMS.GenerateChangeSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a target change set for a currently launched stack and writes it to an Amazon S3 object in the customer’s Amazon S3 bucket.
module Network.AWS.SMS.GenerateChangeSet
  ( -- * Creating a Request
    generateChangeSet,
    GenerateChangeSet,

    -- * Request Lenses
    gcsAppId,
    gcsChangesetFormat,

    -- * Destructuring the Response
    generateChangeSetResponse,
    GenerateChangeSetResponse,

    -- * Response Lenses
    gcsrsS3Location,
    gcsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types

-- | /See:/ 'generateChangeSet' smart constructor.
data GenerateChangeSet = GenerateChangeSet'
  { _gcsAppId ::
      !(Maybe Text),
    _gcsChangesetFormat :: !(Maybe OutputFormat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GenerateChangeSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsAppId' - The ID of the application associated with the change set.
--
-- * 'gcsChangesetFormat' - The format for the change set.
generateChangeSet ::
  GenerateChangeSet
generateChangeSet =
  GenerateChangeSet'
    { _gcsAppId = Nothing,
      _gcsChangesetFormat = Nothing
    }

-- | The ID of the application associated with the change set.
gcsAppId :: Lens' GenerateChangeSet (Maybe Text)
gcsAppId = lens _gcsAppId (\s a -> s {_gcsAppId = a})

-- | The format for the change set.
gcsChangesetFormat :: Lens' GenerateChangeSet (Maybe OutputFormat)
gcsChangesetFormat = lens _gcsChangesetFormat (\s a -> s {_gcsChangesetFormat = a})

instance AWSRequest GenerateChangeSet where
  type Rs GenerateChangeSet = GenerateChangeSetResponse
  request = postJSON sms
  response =
    receiveJSON
      ( \s h x ->
          GenerateChangeSetResponse'
            <$> (x .?> "s3Location") <*> (pure (fromEnum s))
      )

instance Hashable GenerateChangeSet

instance NFData GenerateChangeSet

instance ToHeaders GenerateChangeSet where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSServerMigrationService_V2016_10_24.GenerateChangeSet" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GenerateChangeSet where
  toJSON GenerateChangeSet' {..} =
    object
      ( catMaybes
          [ ("appId" .=) <$> _gcsAppId,
            ("changesetFormat" .=) <$> _gcsChangesetFormat
          ]
      )

instance ToPath GenerateChangeSet where
  toPath = const "/"

instance ToQuery GenerateChangeSet where
  toQuery = const mempty

-- | /See:/ 'generateChangeSetResponse' smart constructor.
data GenerateChangeSetResponse = GenerateChangeSetResponse'
  { _gcsrsS3Location ::
      !(Maybe S3Location),
    _gcsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GenerateChangeSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsrsS3Location' - The location of the Amazon S3 object.
--
-- * 'gcsrsResponseStatus' - -- | The response status code.
generateChangeSetResponse ::
  -- | 'gcsrsResponseStatus'
  Int ->
  GenerateChangeSetResponse
generateChangeSetResponse pResponseStatus_ =
  GenerateChangeSetResponse'
    { _gcsrsS3Location = Nothing,
      _gcsrsResponseStatus = pResponseStatus_
    }

-- | The location of the Amazon S3 object.
gcsrsS3Location :: Lens' GenerateChangeSetResponse (Maybe S3Location)
gcsrsS3Location = lens _gcsrsS3Location (\s a -> s {_gcsrsS3Location = a})

-- | -- | The response status code.
gcsrsResponseStatus :: Lens' GenerateChangeSetResponse Int
gcsrsResponseStatus = lens _gcsrsResponseStatus (\s a -> s {_gcsrsResponseStatus = a})

instance NFData GenerateChangeSetResponse
