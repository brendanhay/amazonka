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
-- Module      : Network.AWS.EFS.PutLifecycleConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables lifecycle management by creating a new @LifecycleConfiguration@ object. A @LifecycleConfiguration@ object defines when files in an Amazon EFS file system are automatically transitioned to the lower-cost EFS Infrequent Access (IA) storage class. A @LifecycleConfiguration@ applies to all files in a file system.
--
--
-- Each Amazon EFS file system supports one lifecycle configuration, which applies to all files in the file system. If a @LifecycleConfiguration@ object already exists for the specified file system, a @PutLifecycleConfiguration@ call modifies the existing configuration. A @PutLifecycleConfiguration@ call with an empty @LifecyclePolicies@ array in the request body deletes any existing @LifecycleConfiguration@ and disables lifecycle management.
--
-- In the request, specify the following:
--
--     * The ID for the file system for which you are creating a lifecycle management configuration.
--
--     * A @LifecyclePolicies@ array of @LifecyclePolicy@ objects that define when files are moved to the IA storage class. The array can contain only one @"TransitionToIA": "AFTER_30_DAYS"@ @LifecyclePolicy@ item.
--
--
--
-- This operation requires permissions for the @elasticfilesystem:PutLifecycleConfiguration@ operation.
--
-- To apply a @LifecycleConfiguration@ object to an encrypted file system, you need the same AWS Key Management Service (AWS KMS) permissions as when you created the encrypted file system.
--
module Network.AWS.EFS.PutLifecycleConfiguration
    (
    -- * Creating a Request
      putLifecycleConfiguration
    , PutLifecycleConfiguration
    -- * Request Lenses
    , plcFileSystemId
    , plcLifecyclePolicies

    -- * Destructuring the Response
    , lifecycleConfigurationDescription
    , LifecycleConfigurationDescription
    -- * Response Lenses
    , lcdLifecyclePolicies
    ) where

import Network.AWS.EFS.Types
import Network.AWS.EFS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putLifecycleConfiguration' smart constructor.
data PutLifecycleConfiguration = PutLifecycleConfiguration'
  { _plcFileSystemId      :: !Text
  , _plcLifecyclePolicies :: ![LifecyclePolicy]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutLifecycleConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plcFileSystemId' - The ID of the file system for which you are creating the @LifecycleConfiguration@ object (String).
--
-- * 'plcLifecyclePolicies' - An array of @LifecyclePolicy@ objects that define the file system's @LifecycleConfiguration@ object. A @LifecycleConfiguration@ object tells lifecycle management when to transition files from the Standard storage class to the Infrequent Access storage class.
putLifecycleConfiguration
    :: Text -- ^ 'plcFileSystemId'
    -> PutLifecycleConfiguration
putLifecycleConfiguration pFileSystemId_ =
  PutLifecycleConfiguration'
    {_plcFileSystemId = pFileSystemId_, _plcLifecyclePolicies = mempty}


-- | The ID of the file system for which you are creating the @LifecycleConfiguration@ object (String).
plcFileSystemId :: Lens' PutLifecycleConfiguration Text
plcFileSystemId = lens _plcFileSystemId (\ s a -> s{_plcFileSystemId = a})

-- | An array of @LifecyclePolicy@ objects that define the file system's @LifecycleConfiguration@ object. A @LifecycleConfiguration@ object tells lifecycle management when to transition files from the Standard storage class to the Infrequent Access storage class.
plcLifecyclePolicies :: Lens' PutLifecycleConfiguration [LifecyclePolicy]
plcLifecyclePolicies = lens _plcLifecyclePolicies (\ s a -> s{_plcLifecyclePolicies = a}) . _Coerce

instance AWSRequest PutLifecycleConfiguration where
        type Rs PutLifecycleConfiguration =
             LifecycleConfigurationDescription
        request = putJSON efs
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable PutLifecycleConfiguration where

instance NFData PutLifecycleConfiguration where

instance ToHeaders PutLifecycleConfiguration where
        toHeaders = const mempty

instance ToJSON PutLifecycleConfiguration where
        toJSON PutLifecycleConfiguration'{..}
          = object
              (catMaybes
                 [Just
                    ("LifecyclePolicies" .= _plcLifecyclePolicies)])

instance ToPath PutLifecycleConfiguration where
        toPath PutLifecycleConfiguration'{..}
          = mconcat
              ["/2015-02-01/file-systems/", toBS _plcFileSystemId,
               "/lifecycle-configuration"]

instance ToQuery PutLifecycleConfiguration where
        toQuery = const mempty
