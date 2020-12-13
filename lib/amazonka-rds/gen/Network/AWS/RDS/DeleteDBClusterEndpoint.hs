{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBClusterEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom endpoint and removes it from an Amazon Aurora DB cluster.
module Network.AWS.RDS.DeleteDBClusterEndpoint
  ( -- * Creating a request
    DeleteDBClusterEndpoint (..),
    mkDeleteDBClusterEndpoint,

    -- ** Request lenses
    ddbceDBClusterEndpointIdentifier,

    -- * Destructuring the response
    DBClusterEndpoint (..),
    mkDBClusterEndpoint,

    -- ** Response lenses
    dceStatus,
    dceDBClusterIdentifier,
    dceDBClusterEndpointARN,
    dceCustomEndpointType,
    dceStaticMembers,
    dceEndpointType,
    dceDBClusterEndpointIdentifier,
    dceEndpoint,
    dceDBClusterEndpointResourceIdentifier,
    dceExcludedMembers,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDBClusterEndpoint' smart constructor.
newtype DeleteDBClusterEndpoint = DeleteDBClusterEndpoint'
  { -- | The identifier associated with the custom endpoint. This parameter is stored as a lowercase string.
    dbClusterEndpointIdentifier :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDBClusterEndpoint' with the minimum fields required to make a request.
--
-- * 'dbClusterEndpointIdentifier' - The identifier associated with the custom endpoint. This parameter is stored as a lowercase string.
mkDeleteDBClusterEndpoint ::
  -- | 'dbClusterEndpointIdentifier'
  Lude.Text ->
  DeleteDBClusterEndpoint
mkDeleteDBClusterEndpoint pDBClusterEndpointIdentifier_ =
  DeleteDBClusterEndpoint'
    { dbClusterEndpointIdentifier =
        pDBClusterEndpointIdentifier_
    }

-- | The identifier associated with the custom endpoint. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dbClusterEndpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddbceDBClusterEndpointIdentifier :: Lens.Lens' DeleteDBClusterEndpoint Lude.Text
ddbceDBClusterEndpointIdentifier = Lens.lens (dbClusterEndpointIdentifier :: DeleteDBClusterEndpoint -> Lude.Text) (\s a -> s {dbClusterEndpointIdentifier = a} :: DeleteDBClusterEndpoint)
{-# DEPRECATED ddbceDBClusterEndpointIdentifier "Use generic-lens or generic-optics with 'dbClusterEndpointIdentifier' instead." #-}

instance Lude.AWSRequest DeleteDBClusterEndpoint where
  type Rs DeleteDBClusterEndpoint = DBClusterEndpoint
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DeleteDBClusterEndpointResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders DeleteDBClusterEndpoint where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDBClusterEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDBClusterEndpoint where
  toQuery DeleteDBClusterEndpoint' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteDBClusterEndpoint" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBClusterEndpointIdentifier" Lude.=: dbClusterEndpointIdentifier
      ]
