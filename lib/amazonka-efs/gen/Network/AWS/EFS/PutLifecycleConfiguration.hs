{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.PutLifecycleConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables lifecycle management by creating a new @LifecycleConfiguration@ object. A @LifecycleConfiguration@ object defines when files in an Amazon EFS file system are automatically transitioned to the lower-cost EFS Infrequent Access (IA) storage class. A @LifecycleConfiguration@ applies to all files in a file system.
--
-- Each Amazon EFS file system supports one lifecycle configuration, which applies to all files in the file system. If a @LifecycleConfiguration@ object already exists for the specified file system, a @PutLifecycleConfiguration@ call modifies the existing configuration. A @PutLifecycleConfiguration@ call with an empty @LifecyclePolicies@ array in the request body deletes any existing @LifecycleConfiguration@ and disables lifecycle management.
-- In the request, specify the following:
--
--     * The ID for the file system for which you are enabling, disabling, or modifying lifecycle management.
--
--
--     * A @LifecyclePolicies@ array of @LifecyclePolicy@ objects that define when files are moved to the IA storage class. The array can contain only one @LifecyclePolicy@ item.
--
--
-- This operation requires permissions for the @elasticfilesystem:PutLifecycleConfiguration@ operation.
-- To apply a @LifecycleConfiguration@ object to an encrypted file system, you need the same AWS Key Management Service (AWS KMS) permissions as when you created the encrypted file system.
module Network.AWS.EFS.PutLifecycleConfiguration
  ( -- * Creating a request
    PutLifecycleConfiguration (..),
    mkPutLifecycleConfiguration,

    -- ** Request lenses
    plcFileSystemId,
    plcLifecyclePolicies,

    -- * Destructuring the response
    LifecycleConfigurationDescription (..),
    mkLifecycleConfigurationDescription,

    -- ** Response lenses
    lcdLifecyclePolicies,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutLifecycleConfiguration' smart constructor.
data PutLifecycleConfiguration = PutLifecycleConfiguration'
  { fileSystemId ::
      Lude.Text,
    lifecyclePolicies :: [LifecyclePolicy]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutLifecycleConfiguration' with the minimum fields required to make a request.
--
-- * 'fileSystemId' - The ID of the file system for which you are creating the @LifecycleConfiguration@ object (String).
-- * 'lifecyclePolicies' - An array of @LifecyclePolicy@ objects that define the file system's @LifecycleConfiguration@ object. A @LifecycleConfiguration@ object tells lifecycle management when to transition files from the Standard storage class to the Infrequent Access storage class.
mkPutLifecycleConfiguration ::
  -- | 'fileSystemId'
  Lude.Text ->
  PutLifecycleConfiguration
mkPutLifecycleConfiguration pFileSystemId_ =
  PutLifecycleConfiguration'
    { fileSystemId = pFileSystemId_,
      lifecyclePolicies = Lude.mempty
    }

-- | The ID of the file system for which you are creating the @LifecycleConfiguration@ object (String).
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plcFileSystemId :: Lens.Lens' PutLifecycleConfiguration Lude.Text
plcFileSystemId = Lens.lens (fileSystemId :: PutLifecycleConfiguration -> Lude.Text) (\s a -> s {fileSystemId = a} :: PutLifecycleConfiguration)
{-# DEPRECATED plcFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | An array of @LifecyclePolicy@ objects that define the file system's @LifecycleConfiguration@ object. A @LifecycleConfiguration@ object tells lifecycle management when to transition files from the Standard storage class to the Infrequent Access storage class.
--
-- /Note:/ Consider using 'lifecyclePolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
plcLifecyclePolicies :: Lens.Lens' PutLifecycleConfiguration [LifecyclePolicy]
plcLifecyclePolicies = Lens.lens (lifecyclePolicies :: PutLifecycleConfiguration -> [LifecyclePolicy]) (\s a -> s {lifecyclePolicies = a} :: PutLifecycleConfiguration)
{-# DEPRECATED plcLifecyclePolicies "Use generic-lens or generic-optics with 'lifecyclePolicies' instead." #-}

instance Lude.AWSRequest PutLifecycleConfiguration where
  type
    Rs PutLifecycleConfiguration =
      LifecycleConfigurationDescription
  request = Req.putJSON efsService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders PutLifecycleConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON PutLifecycleConfiguration where
  toJSON PutLifecycleConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("LifecyclePolicies" Lude..= lifecyclePolicies)]
      )

instance Lude.ToPath PutLifecycleConfiguration where
  toPath PutLifecycleConfiguration' {..} =
    Lude.mconcat
      [ "/2015-02-01/file-systems/",
        Lude.toBS fileSystemId,
        "/lifecycle-configuration"
      ]

instance Lude.ToQuery PutLifecycleConfiguration where
  toQuery = Lude.const Lude.mempty
