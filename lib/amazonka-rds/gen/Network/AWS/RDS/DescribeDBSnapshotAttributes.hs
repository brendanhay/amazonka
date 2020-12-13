{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBSnapshotAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of DB snapshot attribute names and values for a manual DB snapshot.
--
-- When sharing snapshots with other AWS accounts, @DescribeDBSnapshotAttributes@ returns the @restore@ attribute and a list of IDs for the AWS accounts that are authorized to copy or restore the manual DB snapshot. If @all@ is included in the list of values for the @restore@ attribute, then the manual DB snapshot is public and can be copied or restored by all AWS accounts.
-- To add or remove access for an AWS account to copy or restore a manual DB snapshot, or to make the manual DB snapshot public or private, use the @ModifyDBSnapshotAttribute@ API action.
module Network.AWS.RDS.DescribeDBSnapshotAttributes
  ( -- * Creating a request
    DescribeDBSnapshotAttributes (..),
    mkDescribeDBSnapshotAttributes,

    -- ** Request lenses
    ddsaDBSnapshotIdentifier,

    -- * Destructuring the response
    DescribeDBSnapshotAttributesResponse (..),
    mkDescribeDBSnapshotAttributesResponse,

    -- ** Response lenses
    ddsarsDBSnapshotAttributesResult,
    ddsarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeDBSnapshotAttributes' smart constructor.
newtype DescribeDBSnapshotAttributes = DescribeDBSnapshotAttributes'
  { -- | The identifier for the DB snapshot to describe the attributes for.
    dbSnapshotIdentifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBSnapshotAttributes' with the minimum fields required to make a request.
--
-- * 'dbSnapshotIdentifier' - The identifier for the DB snapshot to describe the attributes for.
mkDescribeDBSnapshotAttributes ::
  -- | 'dbSnapshotIdentifier'
  Lude.Text ->
  DescribeDBSnapshotAttributes
mkDescribeDBSnapshotAttributes pDBSnapshotIdentifier_ =
  DescribeDBSnapshotAttributes'
    { dbSnapshotIdentifier =
        pDBSnapshotIdentifier_
    }

-- | The identifier for the DB snapshot to describe the attributes for.
--
-- /Note:/ Consider using 'dbSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsaDBSnapshotIdentifier :: Lens.Lens' DescribeDBSnapshotAttributes Lude.Text
ddsaDBSnapshotIdentifier = Lens.lens (dbSnapshotIdentifier :: DescribeDBSnapshotAttributes -> Lude.Text) (\s a -> s {dbSnapshotIdentifier = a} :: DescribeDBSnapshotAttributes)
{-# DEPRECATED ddsaDBSnapshotIdentifier "Use generic-lens or generic-optics with 'dbSnapshotIdentifier' instead." #-}

instance Lude.AWSRequest DescribeDBSnapshotAttributes where
  type
    Rs DescribeDBSnapshotAttributes =
      DescribeDBSnapshotAttributesResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBSnapshotAttributesResult"
      ( \s h x ->
          DescribeDBSnapshotAttributesResponse'
            Lude.<$> (x Lude..@? "DBSnapshotAttributesResult")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBSnapshotAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBSnapshotAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBSnapshotAttributes where
  toQuery DescribeDBSnapshotAttributes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeDBSnapshotAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBSnapshotIdentifier" Lude.=: dbSnapshotIdentifier
      ]

-- | /See:/ 'mkDescribeDBSnapshotAttributesResponse' smart constructor.
data DescribeDBSnapshotAttributesResponse = DescribeDBSnapshotAttributesResponse'
  { dbSnapshotAttributesResult :: Lude.Maybe DBSnapshotAttributesResult,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBSnapshotAttributesResponse' with the minimum fields required to make a request.
--
-- * 'dbSnapshotAttributesResult' -
-- * 'responseStatus' - The response status code.
mkDescribeDBSnapshotAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBSnapshotAttributesResponse
mkDescribeDBSnapshotAttributesResponse pResponseStatus_ =
  DescribeDBSnapshotAttributesResponse'
    { dbSnapshotAttributesResult =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbSnapshotAttributesResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsarsDBSnapshotAttributesResult :: Lens.Lens' DescribeDBSnapshotAttributesResponse (Lude.Maybe DBSnapshotAttributesResult)
ddsarsDBSnapshotAttributesResult = Lens.lens (dbSnapshotAttributesResult :: DescribeDBSnapshotAttributesResponse -> Lude.Maybe DBSnapshotAttributesResult) (\s a -> s {dbSnapshotAttributesResult = a} :: DescribeDBSnapshotAttributesResponse)
{-# DEPRECATED ddsarsDBSnapshotAttributesResult "Use generic-lens or generic-optics with 'dbSnapshotAttributesResult' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsarsResponseStatus :: Lens.Lens' DescribeDBSnapshotAttributesResponse Lude.Int
ddsarsResponseStatus = Lens.lens (responseStatus :: DescribeDBSnapshotAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBSnapshotAttributesResponse)
{-# DEPRECATED ddsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
