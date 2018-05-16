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
-- Module      : Network.AWS.MigrationHub.DisassociateCreatedArtifact
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a created artifact of an AWS resource with a migration task performed by a migration tool that was previously associated. This API has the following traits:
--
--
--     * A migration user can call the @DisassociateCreatedArtifacts@ operation to disassociate a created AWS Artifact from a migration task.
--
--     * The created artifact name must be provided in ARN (Amazon Resource Name) format which will contain information about type and region; for example: @arn:aws:ec2:us-east-1:488216288981:image/ami-6d0ba87b@ .
--
--     * Examples of the AWS resource behind the created artifact are, AMI's, EC2 instance, or RDS instance, etc.
--
--
--
module Network.AWS.MigrationHub.DisassociateCreatedArtifact
    (
    -- * Creating a Request
      disassociateCreatedArtifact
    , DisassociateCreatedArtifact
    -- * Request Lenses
    , dcaDryRun
    , dcaProgressUpdateStream
    , dcaMigrationTaskName
    , dcaCreatedArtifactName

    -- * Destructuring the Response
    , disassociateCreatedArtifactResponse
    , DisassociateCreatedArtifactResponse
    -- * Response Lenses
    , dcarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types
import Network.AWS.MigrationHub.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateCreatedArtifact' smart constructor.
data DisassociateCreatedArtifact = DisassociateCreatedArtifact'
  { _dcaDryRun               :: !(Maybe Bool)
  , _dcaProgressUpdateStream :: !Text
  , _dcaMigrationTaskName    :: !Text
  , _dcaCreatedArtifactName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateCreatedArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcaDryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- * 'dcaProgressUpdateStream' - The name of the ProgressUpdateStream.
--
-- * 'dcaMigrationTaskName' - Unique identifier that references the migration task to be disassociated with the artifact.
--
-- * 'dcaCreatedArtifactName' - An ARN of the AWS resource related to the migration (e.g., AMI, EC2 instance, RDS instance, etc.)
disassociateCreatedArtifact
    :: Text -- ^ 'dcaProgressUpdateStream'
    -> Text -- ^ 'dcaMigrationTaskName'
    -> Text -- ^ 'dcaCreatedArtifactName'
    -> DisassociateCreatedArtifact
disassociateCreatedArtifact pProgressUpdateStream_ pMigrationTaskName_ pCreatedArtifactName_ =
  DisassociateCreatedArtifact'
    { _dcaDryRun = Nothing
    , _dcaProgressUpdateStream = pProgressUpdateStream_
    , _dcaMigrationTaskName = pMigrationTaskName_
    , _dcaCreatedArtifactName = pCreatedArtifactName_
    }


-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
dcaDryRun :: Lens' DisassociateCreatedArtifact (Maybe Bool)
dcaDryRun = lens _dcaDryRun (\ s a -> s{_dcaDryRun = a})

-- | The name of the ProgressUpdateStream.
dcaProgressUpdateStream :: Lens' DisassociateCreatedArtifact Text
dcaProgressUpdateStream = lens _dcaProgressUpdateStream (\ s a -> s{_dcaProgressUpdateStream = a})

-- | Unique identifier that references the migration task to be disassociated with the artifact.
dcaMigrationTaskName :: Lens' DisassociateCreatedArtifact Text
dcaMigrationTaskName = lens _dcaMigrationTaskName (\ s a -> s{_dcaMigrationTaskName = a})

-- | An ARN of the AWS resource related to the migration (e.g., AMI, EC2 instance, RDS instance, etc.)
dcaCreatedArtifactName :: Lens' DisassociateCreatedArtifact Text
dcaCreatedArtifactName = lens _dcaCreatedArtifactName (\ s a -> s{_dcaCreatedArtifactName = a})

instance AWSRequest DisassociateCreatedArtifact where
        type Rs DisassociateCreatedArtifact =
             DisassociateCreatedArtifactResponse
        request = postJSON migrationHub
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateCreatedArtifactResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DisassociateCreatedArtifact where

instance NFData DisassociateCreatedArtifact where

instance ToHeaders DisassociateCreatedArtifact where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSMigrationHub.DisassociateCreatedArtifact" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateCreatedArtifact where
        toJSON DisassociateCreatedArtifact'{..}
          = object
              (catMaybes
                 [("DryRun" .=) <$> _dcaDryRun,
                  Just
                    ("ProgressUpdateStream" .= _dcaProgressUpdateStream),
                  Just ("MigrationTaskName" .= _dcaMigrationTaskName),
                  Just
                    ("CreatedArtifactName" .= _dcaCreatedArtifactName)])

instance ToPath DisassociateCreatedArtifact where
        toPath = const "/"

instance ToQuery DisassociateCreatedArtifact where
        toQuery = const mempty

-- | /See:/ 'disassociateCreatedArtifactResponse' smart constructor.
newtype DisassociateCreatedArtifactResponse = DisassociateCreatedArtifactResponse'
  { _dcarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateCreatedArtifactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcarsResponseStatus' - -- | The response status code.
disassociateCreatedArtifactResponse
    :: Int -- ^ 'dcarsResponseStatus'
    -> DisassociateCreatedArtifactResponse
disassociateCreatedArtifactResponse pResponseStatus_ =
  DisassociateCreatedArtifactResponse' {_dcarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dcarsResponseStatus :: Lens' DisassociateCreatedArtifactResponse Int
dcarsResponseStatus = lens _dcarsResponseStatus (\ s a -> s{_dcarsResponseStatus = a})

instance NFData DisassociateCreatedArtifactResponse
         where
