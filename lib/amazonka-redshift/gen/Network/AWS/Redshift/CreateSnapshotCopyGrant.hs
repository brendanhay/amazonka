{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateSnapshotCopyGrant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot copy grant that permits Amazon Redshift to use a customer master key (CMK) from AWS Key Management Service (AWS KMS) to encrypt copied snapshots in a destination region.
--
-- For more information about managing snapshot copy grants, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html Amazon Redshift Database Encryption> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.CreateSnapshotCopyGrant
  ( -- * Creating a request
    CreateSnapshotCopyGrant (..),
    mkCreateSnapshotCopyGrant,

    -- ** Request lenses
    cscgKMSKeyId,
    cscgTags,
    cscgSnapshotCopyGrantName,

    -- * Destructuring the response
    CreateSnapshotCopyGrantResponse (..),
    mkCreateSnapshotCopyGrantResponse,

    -- ** Response lenses
    cscgrsSnapshotCopyGrant,
    cscgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The result of the @CreateSnapshotCopyGrant@ action.
--
-- /See:/ 'mkCreateSnapshotCopyGrant' smart constructor.
data CreateSnapshotCopyGrant = CreateSnapshotCopyGrant'
  { kmsKeyId ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    snapshotCopyGrantName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSnapshotCopyGrant' with the minimum fields required to make a request.
--
-- * 'kmsKeyId' - The unique identifier of the customer master key (CMK) to which to grant Amazon Redshift permission. If no key is specified, the default key is used.
-- * 'snapshotCopyGrantName' - The name of the snapshot copy grant. This name must be unique in the region for the AWS account.
--
-- Constraints:
--
--     * Must contain from 1 to 63 alphanumeric characters or hyphens.
--
--
--     * Alphabetic characters must be lowercase.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
--     * Must be unique for all clusters within an AWS account.
--
--
-- * 'tags' - A list of tag instances.
mkCreateSnapshotCopyGrant ::
  -- | 'snapshotCopyGrantName'
  Lude.Text ->
  CreateSnapshotCopyGrant
mkCreateSnapshotCopyGrant pSnapshotCopyGrantName_ =
  CreateSnapshotCopyGrant'
    { kmsKeyId = Lude.Nothing,
      tags = Lude.Nothing,
      snapshotCopyGrantName = pSnapshotCopyGrantName_
    }

-- | The unique identifier of the customer master key (CMK) to which to grant Amazon Redshift permission. If no key is specified, the default key is used.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscgKMSKeyId :: Lens.Lens' CreateSnapshotCopyGrant (Lude.Maybe Lude.Text)
cscgKMSKeyId = Lens.lens (kmsKeyId :: CreateSnapshotCopyGrant -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateSnapshotCopyGrant)
{-# DEPRECATED cscgKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscgTags :: Lens.Lens' CreateSnapshotCopyGrant (Lude.Maybe [Tag])
cscgTags = Lens.lens (tags :: CreateSnapshotCopyGrant -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateSnapshotCopyGrant)
{-# DEPRECATED cscgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the snapshot copy grant. This name must be unique in the region for the AWS account.
--
-- Constraints:
--
--     * Must contain from 1 to 63 alphanumeric characters or hyphens.
--
--
--     * Alphabetic characters must be lowercase.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
--     * Must be unique for all clusters within an AWS account.
--
--
--
-- /Note:/ Consider using 'snapshotCopyGrantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscgSnapshotCopyGrantName :: Lens.Lens' CreateSnapshotCopyGrant Lude.Text
cscgSnapshotCopyGrantName = Lens.lens (snapshotCopyGrantName :: CreateSnapshotCopyGrant -> Lude.Text) (\s a -> s {snapshotCopyGrantName = a} :: CreateSnapshotCopyGrant)
{-# DEPRECATED cscgSnapshotCopyGrantName "Use generic-lens or generic-optics with 'snapshotCopyGrantName' instead." #-}

instance Lude.AWSRequest CreateSnapshotCopyGrant where
  type Rs CreateSnapshotCopyGrant = CreateSnapshotCopyGrantResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "CreateSnapshotCopyGrantResult"
      ( \s h x ->
          CreateSnapshotCopyGrantResponse'
            Lude.<$> (x Lude..@? "SnapshotCopyGrant")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSnapshotCopyGrant where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateSnapshotCopyGrant where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSnapshotCopyGrant where
  toQuery CreateSnapshotCopyGrant' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateSnapshotCopyGrant" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "KmsKeyId" Lude.=: kmsKeyId,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "SnapshotCopyGrantName" Lude.=: snapshotCopyGrantName
      ]

-- | /See:/ 'mkCreateSnapshotCopyGrantResponse' smart constructor.
data CreateSnapshotCopyGrantResponse = CreateSnapshotCopyGrantResponse'
  { snapshotCopyGrant ::
      Lude.Maybe
        SnapshotCopyGrant,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSnapshotCopyGrantResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'snapshotCopyGrant' - Undocumented field.
mkCreateSnapshotCopyGrantResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSnapshotCopyGrantResponse
mkCreateSnapshotCopyGrantResponse pResponseStatus_ =
  CreateSnapshotCopyGrantResponse'
    { snapshotCopyGrant =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshotCopyGrant' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscgrsSnapshotCopyGrant :: Lens.Lens' CreateSnapshotCopyGrantResponse (Lude.Maybe SnapshotCopyGrant)
cscgrsSnapshotCopyGrant = Lens.lens (snapshotCopyGrant :: CreateSnapshotCopyGrantResponse -> Lude.Maybe SnapshotCopyGrant) (\s a -> s {snapshotCopyGrant = a} :: CreateSnapshotCopyGrantResponse)
{-# DEPRECATED cscgrsSnapshotCopyGrant "Use generic-lens or generic-optics with 'snapshotCopyGrant' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscgrsResponseStatus :: Lens.Lens' CreateSnapshotCopyGrantResponse Lude.Int
cscgrsResponseStatus = Lens.lens (responseStatus :: CreateSnapshotCopyGrantResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSnapshotCopyGrantResponse)
{-# DEPRECATED cscgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
