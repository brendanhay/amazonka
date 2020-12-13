{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBClusterEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom endpoint and associates it with an Amazon Aurora DB cluster.
module Network.AWS.RDS.CreateDBClusterEndpoint
  ( -- * Creating a request
    CreateDBClusterEndpoint (..),
    mkCreateDBClusterEndpoint,

    -- ** Request lenses
    cdceDBClusterIdentifier,
    cdceStaticMembers,
    cdceEndpointType,
    cdceDBClusterEndpointIdentifier,
    cdceExcludedMembers,
    cdceTags,

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

-- | /See:/ 'mkCreateDBClusterEndpoint' smart constructor.
data CreateDBClusterEndpoint = CreateDBClusterEndpoint'
  { -- | The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
    dbClusterIdentifier :: Lude.Text,
    -- | List of DB instance identifiers that are part of the custom endpoint group.
    staticMembers :: Lude.Maybe [Lude.Text],
    -- | The type of the endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
    endpointType :: Lude.Text,
    -- | The identifier to use for the new endpoint. This parameter is stored as a lowercase string.
    dbClusterEndpointIdentifier :: Lude.Text,
    -- | List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
    excludedMembers :: Lude.Maybe [Lude.Text],
    -- | The tags to be assigned to the Amazon RDS resource.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBClusterEndpoint' with the minimum fields required to make a request.
--
-- * 'dbClusterIdentifier' - The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
-- * 'staticMembers' - List of DB instance identifiers that are part of the custom endpoint group.
-- * 'endpointType' - The type of the endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
-- * 'dbClusterEndpointIdentifier' - The identifier to use for the new endpoint. This parameter is stored as a lowercase string.
-- * 'excludedMembers' - List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
-- * 'tags' - The tags to be assigned to the Amazon RDS resource.
mkCreateDBClusterEndpoint ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  -- | 'endpointType'
  Lude.Text ->
  -- | 'dbClusterEndpointIdentifier'
  Lude.Text ->
  CreateDBClusterEndpoint
mkCreateDBClusterEndpoint
  pDBClusterIdentifier_
  pEndpointType_
  pDBClusterEndpointIdentifier_ =
    CreateDBClusterEndpoint'
      { dbClusterIdentifier =
          pDBClusterIdentifier_,
        staticMembers = Lude.Nothing,
        endpointType = pEndpointType_,
        dbClusterEndpointIdentifier = pDBClusterEndpointIdentifier_,
        excludedMembers = Lude.Nothing,
        tags = Lude.Nothing
      }

-- | The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdceDBClusterIdentifier :: Lens.Lens' CreateDBClusterEndpoint Lude.Text
cdceDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: CreateDBClusterEndpoint -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: CreateDBClusterEndpoint)
{-# DEPRECATED cdceDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | List of DB instance identifiers that are part of the custom endpoint group.
--
-- /Note:/ Consider using 'staticMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdceStaticMembers :: Lens.Lens' CreateDBClusterEndpoint (Lude.Maybe [Lude.Text])
cdceStaticMembers = Lens.lens (staticMembers :: CreateDBClusterEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {staticMembers = a} :: CreateDBClusterEndpoint)
{-# DEPRECATED cdceStaticMembers "Use generic-lens or generic-optics with 'staticMembers' instead." #-}

-- | The type of the endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdceEndpointType :: Lens.Lens' CreateDBClusterEndpoint Lude.Text
cdceEndpointType = Lens.lens (endpointType :: CreateDBClusterEndpoint -> Lude.Text) (\s a -> s {endpointType = a} :: CreateDBClusterEndpoint)
{-# DEPRECATED cdceEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

-- | The identifier to use for the new endpoint. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dbClusterEndpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdceDBClusterEndpointIdentifier :: Lens.Lens' CreateDBClusterEndpoint Lude.Text
cdceDBClusterEndpointIdentifier = Lens.lens (dbClusterEndpointIdentifier :: CreateDBClusterEndpoint -> Lude.Text) (\s a -> s {dbClusterEndpointIdentifier = a} :: CreateDBClusterEndpoint)
{-# DEPRECATED cdceDBClusterEndpointIdentifier "Use generic-lens or generic-optics with 'dbClusterEndpointIdentifier' instead." #-}

-- | List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
--
-- /Note:/ Consider using 'excludedMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdceExcludedMembers :: Lens.Lens' CreateDBClusterEndpoint (Lude.Maybe [Lude.Text])
cdceExcludedMembers = Lens.lens (excludedMembers :: CreateDBClusterEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {excludedMembers = a} :: CreateDBClusterEndpoint)
{-# DEPRECATED cdceExcludedMembers "Use generic-lens or generic-optics with 'excludedMembers' instead." #-}

-- | The tags to be assigned to the Amazon RDS resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdceTags :: Lens.Lens' CreateDBClusterEndpoint (Lude.Maybe [Tag])
cdceTags = Lens.lens (tags :: CreateDBClusterEndpoint -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDBClusterEndpoint)
{-# DEPRECATED cdceTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateDBClusterEndpoint where
  type Rs CreateDBClusterEndpoint = DBClusterEndpoint
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CreateDBClusterEndpointResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders CreateDBClusterEndpoint where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateDBClusterEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDBClusterEndpoint where
  toQuery CreateDBClusterEndpoint' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateDBClusterEndpoint" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier,
        "StaticMembers"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> staticMembers),
        "EndpointType" Lude.=: endpointType,
        "DBClusterEndpointIdentifier" Lude.=: dbClusterEndpointIdentifier,
        "ExcludedMembers"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> excludedMembers),
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags)
      ]
