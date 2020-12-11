{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBClusterSnapshotAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of DB cluster snapshot attribute names and values for a manual DB cluster snapshot.
--
-- When sharing snapshots with other AWS accounts, @DescribeDBClusterSnapshotAttributes@ returns the @restore@ attribute and a list of IDs for the AWS accounts that are authorized to copy or restore the manual DB cluster snapshot. If @all@ is included in the list of values for the @restore@ attribute, then the manual DB cluster snapshot is public and can be copied or restored by all AWS accounts.
-- To add or remove access for an AWS account to copy or restore a manual DB cluster snapshot, or to make the manual DB cluster snapshot public or private, use the @ModifyDBClusterSnapshotAttribute@ API action.
module Network.AWS.RDS.DescribeDBClusterSnapshotAttributes
  ( -- * Creating a request
    DescribeDBClusterSnapshotAttributes (..),
    mkDescribeDBClusterSnapshotAttributes,

    -- ** Request lenses
    ddcsaDBClusterSnapshotIdentifier,

    -- * Destructuring the response
    DescribeDBClusterSnapshotAttributesResponse (..),
    mkDescribeDBClusterSnapshotAttributesResponse,

    -- ** Response lenses
    ddcsarsDBClusterSnapshotAttributesResult,
    ddcsarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeDBClusterSnapshotAttributes' smart constructor.
newtype DescribeDBClusterSnapshotAttributes = DescribeDBClusterSnapshotAttributes'
  { dbClusterSnapshotIdentifier ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBClusterSnapshotAttributes' with the minimum fields required to make a request.
--
-- * 'dbClusterSnapshotIdentifier' - The identifier for the DB cluster snapshot to describe the attributes for.
mkDescribeDBClusterSnapshotAttributes ::
  -- | 'dbClusterSnapshotIdentifier'
  Lude.Text ->
  DescribeDBClusterSnapshotAttributes
mkDescribeDBClusterSnapshotAttributes pDBClusterSnapshotIdentifier_ =
  DescribeDBClusterSnapshotAttributes'
    { dbClusterSnapshotIdentifier =
        pDBClusterSnapshotIdentifier_
    }

-- | The identifier for the DB cluster snapshot to describe the attributes for.
--
-- /Note:/ Consider using 'dbClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcsaDBClusterSnapshotIdentifier :: Lens.Lens' DescribeDBClusterSnapshotAttributes Lude.Text
ddcsaDBClusterSnapshotIdentifier = Lens.lens (dbClusterSnapshotIdentifier :: DescribeDBClusterSnapshotAttributes -> Lude.Text) (\s a -> s {dbClusterSnapshotIdentifier = a} :: DescribeDBClusterSnapshotAttributes)
{-# DEPRECATED ddcsaDBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'dbClusterSnapshotIdentifier' instead." #-}

instance Lude.AWSRequest DescribeDBClusterSnapshotAttributes where
  type
    Rs DescribeDBClusterSnapshotAttributes =
      DescribeDBClusterSnapshotAttributesResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeDBClusterSnapshotAttributesResult"
      ( \s h x ->
          DescribeDBClusterSnapshotAttributesResponse'
            Lude.<$> (x Lude..@? "DBClusterSnapshotAttributesResult")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDBClusterSnapshotAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeDBClusterSnapshotAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDBClusterSnapshotAttributes where
  toQuery DescribeDBClusterSnapshotAttributes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeDBClusterSnapshotAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBClusterSnapshotIdentifier" Lude.=: dbClusterSnapshotIdentifier
      ]

-- | /See:/ 'mkDescribeDBClusterSnapshotAttributesResponse' smart constructor.
data DescribeDBClusterSnapshotAttributesResponse = DescribeDBClusterSnapshotAttributesResponse'
  { dbClusterSnapshotAttributesResult ::
      Lude.Maybe
        DBClusterSnapshotAttributesResult,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDBClusterSnapshotAttributesResponse' with the minimum fields required to make a request.
--
-- * 'dbClusterSnapshotAttributesResult' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeDBClusterSnapshotAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDBClusterSnapshotAttributesResponse
mkDescribeDBClusterSnapshotAttributesResponse pResponseStatus_ =
  DescribeDBClusterSnapshotAttributesResponse'
    { dbClusterSnapshotAttributesResult =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbClusterSnapshotAttributesResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcsarsDBClusterSnapshotAttributesResult :: Lens.Lens' DescribeDBClusterSnapshotAttributesResponse (Lude.Maybe DBClusterSnapshotAttributesResult)
ddcsarsDBClusterSnapshotAttributesResult = Lens.lens (dbClusterSnapshotAttributesResult :: DescribeDBClusterSnapshotAttributesResponse -> Lude.Maybe DBClusterSnapshotAttributesResult) (\s a -> s {dbClusterSnapshotAttributesResult = a} :: DescribeDBClusterSnapshotAttributesResponse)
{-# DEPRECATED ddcsarsDBClusterSnapshotAttributesResult "Use generic-lens or generic-optics with 'dbClusterSnapshotAttributesResult' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcsarsResponseStatus :: Lens.Lens' DescribeDBClusterSnapshotAttributesResponse Lude.Int
ddcsarsResponseStatus = Lens.lens (responseStatus :: DescribeDBClusterSnapshotAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDBClusterSnapshotAttributesResponse)
{-# DEPRECATED ddcsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
