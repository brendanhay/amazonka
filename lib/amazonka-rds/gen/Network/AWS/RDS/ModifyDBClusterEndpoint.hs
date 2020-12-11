{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBClusterEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the properties of an endpoint in an Amazon Aurora DB cluster.
module Network.AWS.RDS.ModifyDBClusterEndpoint
  ( -- * Creating a request
    ModifyDBClusterEndpoint (..),
    mkModifyDBClusterEndpoint,

    -- ** Request lenses
    mdceStaticMembers,
    mdceEndpointType,
    mdceExcludedMembers,
    mdceDBClusterEndpointIdentifier,

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

-- | /See:/ 'mkModifyDBClusterEndpoint' smart constructor.
data ModifyDBClusterEndpoint = ModifyDBClusterEndpoint'
  { staticMembers ::
      Lude.Maybe [Lude.Text],
    endpointType :: Lude.Maybe Lude.Text,
    excludedMembers :: Lude.Maybe [Lude.Text],
    dbClusterEndpointIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyDBClusterEndpoint' with the minimum fields required to make a request.
--
-- * 'dbClusterEndpointIdentifier' - The identifier of the endpoint to modify. This parameter is stored as a lowercase string.
-- * 'endpointType' - The type of the endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
-- * 'excludedMembers' - List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
-- * 'staticMembers' - List of DB instance identifiers that are part of the custom endpoint group.
mkModifyDBClusterEndpoint ::
  -- | 'dbClusterEndpointIdentifier'
  Lude.Text ->
  ModifyDBClusterEndpoint
mkModifyDBClusterEndpoint pDBClusterEndpointIdentifier_ =
  ModifyDBClusterEndpoint'
    { staticMembers = Lude.Nothing,
      endpointType = Lude.Nothing,
      excludedMembers = Lude.Nothing,
      dbClusterEndpointIdentifier = pDBClusterEndpointIdentifier_
    }

-- | List of DB instance identifiers that are part of the custom endpoint group.
--
-- /Note:/ Consider using 'staticMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdceStaticMembers :: Lens.Lens' ModifyDBClusterEndpoint (Lude.Maybe [Lude.Text])
mdceStaticMembers = Lens.lens (staticMembers :: ModifyDBClusterEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {staticMembers = a} :: ModifyDBClusterEndpoint)
{-# DEPRECATED mdceStaticMembers "Use generic-lens or generic-optics with 'staticMembers' instead." #-}

-- | The type of the endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdceEndpointType :: Lens.Lens' ModifyDBClusterEndpoint (Lude.Maybe Lude.Text)
mdceEndpointType = Lens.lens (endpointType :: ModifyDBClusterEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {endpointType = a} :: ModifyDBClusterEndpoint)
{-# DEPRECATED mdceEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

-- | List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
--
-- /Note:/ Consider using 'excludedMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdceExcludedMembers :: Lens.Lens' ModifyDBClusterEndpoint (Lude.Maybe [Lude.Text])
mdceExcludedMembers = Lens.lens (excludedMembers :: ModifyDBClusterEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {excludedMembers = a} :: ModifyDBClusterEndpoint)
{-# DEPRECATED mdceExcludedMembers "Use generic-lens or generic-optics with 'excludedMembers' instead." #-}

-- | The identifier of the endpoint to modify. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dbClusterEndpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdceDBClusterEndpointIdentifier :: Lens.Lens' ModifyDBClusterEndpoint Lude.Text
mdceDBClusterEndpointIdentifier = Lens.lens (dbClusterEndpointIdentifier :: ModifyDBClusterEndpoint -> Lude.Text) (\s a -> s {dbClusterEndpointIdentifier = a} :: ModifyDBClusterEndpoint)
{-# DEPRECATED mdceDBClusterEndpointIdentifier "Use generic-lens or generic-optics with 'dbClusterEndpointIdentifier' instead." #-}

instance Lude.AWSRequest ModifyDBClusterEndpoint where
  type Rs ModifyDBClusterEndpoint = DBClusterEndpoint
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ModifyDBClusterEndpointResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders ModifyDBClusterEndpoint where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyDBClusterEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyDBClusterEndpoint where
  toQuery ModifyDBClusterEndpoint' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyDBClusterEndpoint" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "StaticMembers"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> staticMembers),
        "EndpointType" Lude.=: endpointType,
        "ExcludedMembers"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> excludedMembers),
        "DBClusterEndpointIdentifier" Lude.=: dbClusterEndpointIdentifier
      ]
