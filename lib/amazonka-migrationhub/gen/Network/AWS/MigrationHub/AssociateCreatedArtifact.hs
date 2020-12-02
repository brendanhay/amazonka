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
-- Module      : Network.AWS.MigrationHub.AssociateCreatedArtifact
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a created artifact of an AWS cloud resource, the target receiving the migration, with the migration task performed by a migration tool. This API has the following traits:
--
--
--     * Migration tools can call the @AssociateCreatedArtifact@ operation to indicate which AWS artifact is associated with a migration task.
--
--     * The created artifact name must be provided in ARN (Amazon Resource Name) format which will contain information about type and region; for example: @arn:aws:ec2:us-east-1:488216288981:image/ami-6d0ba87b@ .
--
--     * Examples of the AWS resource behind the created artifact are, AMI's, EC2 instance, or DMS endpoint, etc.
--
--
--
module Network.AWS.MigrationHub.AssociateCreatedArtifact
    (
    -- * Creating a Request
      associateCreatedArtifact
    , AssociateCreatedArtifact
    -- * Request Lenses
    , acaDryRun
    , acaProgressUpdateStream
    , acaMigrationTaskName
    , acaCreatedArtifact

    -- * Destructuring the Response
    , associateCreatedArtifactResponse
    , AssociateCreatedArtifactResponse
    -- * Response Lenses
    , acarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types
import Network.AWS.MigrationHub.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateCreatedArtifact' smart constructor.
data AssociateCreatedArtifact = AssociateCreatedArtifact'
  { _acaDryRun               :: !(Maybe Bool)
  , _acaProgressUpdateStream :: !Text
  , _acaMigrationTaskName    :: !Text
  , _acaCreatedArtifact      :: !CreatedArtifact
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateCreatedArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acaDryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- * 'acaProgressUpdateStream' - The name of the ProgressUpdateStream.
--
-- * 'acaMigrationTaskName' - Unique identifier that references the migration task.
--
-- * 'acaCreatedArtifact' - An ARN of the AWS resource related to the migration (e.g., AMI, EC2 instance, RDS instance, etc.)
associateCreatedArtifact
    :: Text -- ^ 'acaProgressUpdateStream'
    -> Text -- ^ 'acaMigrationTaskName'
    -> CreatedArtifact -- ^ 'acaCreatedArtifact'
    -> AssociateCreatedArtifact
associateCreatedArtifact pProgressUpdateStream_ pMigrationTaskName_ pCreatedArtifact_ =
  AssociateCreatedArtifact'
    { _acaDryRun = Nothing
    , _acaProgressUpdateStream = pProgressUpdateStream_
    , _acaMigrationTaskName = pMigrationTaskName_
    , _acaCreatedArtifact = pCreatedArtifact_
    }


-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
acaDryRun :: Lens' AssociateCreatedArtifact (Maybe Bool)
acaDryRun = lens _acaDryRun (\ s a -> s{_acaDryRun = a})

-- | The name of the ProgressUpdateStream.
acaProgressUpdateStream :: Lens' AssociateCreatedArtifact Text
acaProgressUpdateStream = lens _acaProgressUpdateStream (\ s a -> s{_acaProgressUpdateStream = a})

-- | Unique identifier that references the migration task.
acaMigrationTaskName :: Lens' AssociateCreatedArtifact Text
acaMigrationTaskName = lens _acaMigrationTaskName (\ s a -> s{_acaMigrationTaskName = a})

-- | An ARN of the AWS resource related to the migration (e.g., AMI, EC2 instance, RDS instance, etc.)
acaCreatedArtifact :: Lens' AssociateCreatedArtifact CreatedArtifact
acaCreatedArtifact = lens _acaCreatedArtifact (\ s a -> s{_acaCreatedArtifact = a})

instance AWSRequest AssociateCreatedArtifact where
        type Rs AssociateCreatedArtifact =
             AssociateCreatedArtifactResponse
        request = postJSON migrationHub
        response
          = receiveEmpty
              (\ s h x ->
                 AssociateCreatedArtifactResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AssociateCreatedArtifact where

instance NFData AssociateCreatedArtifact where

instance ToHeaders AssociateCreatedArtifact where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSMigrationHub.AssociateCreatedArtifact" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateCreatedArtifact where
        toJSON AssociateCreatedArtifact'{..}
          = object
              (catMaybes
                 [("DryRun" .=) <$> _acaDryRun,
                  Just
                    ("ProgressUpdateStream" .= _acaProgressUpdateStream),
                  Just ("MigrationTaskName" .= _acaMigrationTaskName),
                  Just ("CreatedArtifact" .= _acaCreatedArtifact)])

instance ToPath AssociateCreatedArtifact where
        toPath = const "/"

instance ToQuery AssociateCreatedArtifact where
        toQuery = const mempty

-- | /See:/ 'associateCreatedArtifactResponse' smart constructor.
newtype AssociateCreatedArtifactResponse = AssociateCreatedArtifactResponse'
  { _acarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateCreatedArtifactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acarsResponseStatus' - -- | The response status code.
associateCreatedArtifactResponse
    :: Int -- ^ 'acarsResponseStatus'
    -> AssociateCreatedArtifactResponse
associateCreatedArtifactResponse pResponseStatus_ =
  AssociateCreatedArtifactResponse' {_acarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
acarsResponseStatus :: Lens' AssociateCreatedArtifactResponse Int
acarsResponseStatus = lens _acarsResponseStatus (\ s a -> s{_acarsResponseStatus = a})

instance NFData AssociateCreatedArtifactResponse
         where
