{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.PutFileSystemPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies an Amazon EFS @FileSystemPolicy@ to an Amazon EFS file system. A file system policy is an IAM resource-based policy and can contain multiple policy statements. A file system always has exactly one file system policy, which can be the default policy or an explicit policy set or updated using this API operation. When an explicit policy is set, it overrides the default policy. For more information about the default file system policy, see <https://docs.aws.amazon.com/efs/latest/ug/iam-access-control-nfs-efs.html#default-filesystempolicy Default EFS File System Policy> .
--
-- This operation requires permissions for the @elasticfilesystem:PutFileSystemPolicy@ action.
module Network.AWS.EFS.PutFileSystemPolicy
  ( -- * Creating a request
    PutFileSystemPolicy (..),
    mkPutFileSystemPolicy,

    -- ** Request lenses
    pfspBypassPolicyLockoutSafetyCheck,
    pfspFileSystemId,
    pfspPolicy,

    -- * Destructuring the response
    FileSystemPolicyDescription (..),
    mkFileSystemPolicyDescription,

    -- ** Response lenses
    fspdFileSystemId,
    fspdPolicy,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutFileSystemPolicy' smart constructor.
data PutFileSystemPolicy = PutFileSystemPolicy'
  { bypassPolicyLockoutSafetyCheck ::
      Lude.Maybe Lude.Bool,
    fileSystemId :: Lude.Text,
    policy :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutFileSystemPolicy' with the minimum fields required to make a request.
--
-- * 'bypassPolicyLockoutSafetyCheck' - (Optional) A flag to indicate whether to bypass the @FileSystemPolicy@ lockout safety check. The policy lockout safety check determines whether the policy in the request will prevent the principal making the request will be locked out from making future @PutFileSystemPolicy@ requests on the file system. Set @BypassPolicyLockoutSafetyCheck@ to @True@ only when you intend to prevent the principal that is making the request from making a subsequent @PutFileSystemPolicy@ request on the file system. The default value is False.
-- * 'fileSystemId' - The ID of the EFS file system that you want to create or update the @FileSystemPolicy@ for.
-- * 'policy' - The @FileSystemPolicy@ that you're creating. Accepts a JSON formatted policy definition. To find out more about the elements that make up a file system policy, see <https://docs.aws.amazon.com/efs/latest/ug/access-control-overview.html#access-control-manage-access-intro-resource-policies EFS Resource-based Policies> .
mkPutFileSystemPolicy ::
  -- | 'fileSystemId'
  Lude.Text ->
  -- | 'policy'
  Lude.Text ->
  PutFileSystemPolicy
mkPutFileSystemPolicy pFileSystemId_ pPolicy_ =
  PutFileSystemPolicy'
    { bypassPolicyLockoutSafetyCheck =
        Lude.Nothing,
      fileSystemId = pFileSystemId_,
      policy = pPolicy_
    }

-- | (Optional) A flag to indicate whether to bypass the @FileSystemPolicy@ lockout safety check. The policy lockout safety check determines whether the policy in the request will prevent the principal making the request will be locked out from making future @PutFileSystemPolicy@ requests on the file system. Set @BypassPolicyLockoutSafetyCheck@ to @True@ only when you intend to prevent the principal that is making the request from making a subsequent @PutFileSystemPolicy@ request on the file system. The default value is False.
--
-- /Note:/ Consider using 'bypassPolicyLockoutSafetyCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfspBypassPolicyLockoutSafetyCheck :: Lens.Lens' PutFileSystemPolicy (Lude.Maybe Lude.Bool)
pfspBypassPolicyLockoutSafetyCheck = Lens.lens (bypassPolicyLockoutSafetyCheck :: PutFileSystemPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {bypassPolicyLockoutSafetyCheck = a} :: PutFileSystemPolicy)
{-# DEPRECATED pfspBypassPolicyLockoutSafetyCheck "Use generic-lens or generic-optics with 'bypassPolicyLockoutSafetyCheck' instead." #-}

-- | The ID of the EFS file system that you want to create or update the @FileSystemPolicy@ for.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfspFileSystemId :: Lens.Lens' PutFileSystemPolicy Lude.Text
pfspFileSystemId = Lens.lens (fileSystemId :: PutFileSystemPolicy -> Lude.Text) (\s a -> s {fileSystemId = a} :: PutFileSystemPolicy)
{-# DEPRECATED pfspFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

-- | The @FileSystemPolicy@ that you're creating. Accepts a JSON formatted policy definition. To find out more about the elements that make up a file system policy, see <https://docs.aws.amazon.com/efs/latest/ug/access-control-overview.html#access-control-manage-access-intro-resource-policies EFS Resource-based Policies> .
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfspPolicy :: Lens.Lens' PutFileSystemPolicy Lude.Text
pfspPolicy = Lens.lens (policy :: PutFileSystemPolicy -> Lude.Text) (\s a -> s {policy = a} :: PutFileSystemPolicy)
{-# DEPRECATED pfspPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

instance Lude.AWSRequest PutFileSystemPolicy where
  type Rs PutFileSystemPolicy = FileSystemPolicyDescription
  request = Req.putJSON efsService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders PutFileSystemPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON PutFileSystemPolicy where
  toJSON PutFileSystemPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BypassPolicyLockoutSafetyCheck" Lude..=)
              Lude.<$> bypassPolicyLockoutSafetyCheck,
            Lude.Just ("Policy" Lude..= policy)
          ]
      )

instance Lude.ToPath PutFileSystemPolicy where
  toPath PutFileSystemPolicy' {..} =
    Lude.mconcat
      ["/2015-02-01/file-systems/", Lude.toBS fileSystemId, "/policy"]

instance Lude.ToQuery PutFileSystemPolicy where
  toQuery = Lude.const Lude.mempty
