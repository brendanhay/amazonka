{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cdceStaticMembers,
    cdceExcludedMembers,
    cdceTags,
    cdceDBClusterIdentifier,
    cdceDBClusterEndpointIdentifier,
    cdceEndpointType,

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
  { staticMembers ::
      Lude.Maybe [Lude.Text],
    excludedMembers :: Lude.Maybe [Lude.Text],
    tags :: Lude.Maybe [Tag],
    dbClusterIdentifier :: Lude.Text,
    dbClusterEndpointIdentifier :: Lude.Text,
    endpointType :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBClusterEndpoint' with the minimum fields required to make a request.
--
-- * 'dbClusterEndpointIdentifier' - The identifier to use for the new endpoint. This parameter is stored as a lowercase string.
-- * 'dbClusterIdentifier' - The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
-- * 'endpointType' - The type of the endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
-- * 'excludedMembers' - List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
-- * 'staticMembers' - List of DB instance identifiers that are part of the custom endpoint group.
-- * 'tags' - The tags to be assigned to the Amazon RDS resource.
mkCreateDBClusterEndpoint ::
  -- | 'dbClusterIdentifier'
  Lude.Text ->
  -- | 'dbClusterEndpointIdentifier'
  Lude.Text ->
  -- | 'endpointType'
  Lude.Text ->
  CreateDBClusterEndpoint
mkCreateDBClusterEndpoint
  pDBClusterIdentifier_
  pDBClusterEndpointIdentifier_
  pEndpointType_ =
    CreateDBClusterEndpoint'
      { staticMembers = Lude.Nothing,
        excludedMembers = Lude.Nothing,
        tags = Lude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        dbClusterEndpointIdentifier = pDBClusterEndpointIdentifier_,
        endpointType = pEndpointType_
      }

-- | List of DB instance identifiers that are part of the custom endpoint group.
--
-- /Note:/ Consider using 'staticMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdceStaticMembers :: Lens.Lens' CreateDBClusterEndpoint (Lude.Maybe [Lude.Text])
cdceStaticMembers = Lens.lens (staticMembers :: CreateDBClusterEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {staticMembers = a} :: CreateDBClusterEndpoint)
{-# DEPRECATED cdceStaticMembers "Use generic-lens or generic-optics with 'staticMembers' instead." #-}

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

-- | The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdceDBClusterIdentifier :: Lens.Lens' CreateDBClusterEndpoint Lude.Text
cdceDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: CreateDBClusterEndpoint -> Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: CreateDBClusterEndpoint)
{-# DEPRECATED cdceDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | The identifier to use for the new endpoint. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dbClusterEndpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdceDBClusterEndpointIdentifier :: Lens.Lens' CreateDBClusterEndpoint Lude.Text
cdceDBClusterEndpointIdentifier = Lens.lens (dbClusterEndpointIdentifier :: CreateDBClusterEndpoint -> Lude.Text) (\s a -> s {dbClusterEndpointIdentifier = a} :: CreateDBClusterEndpoint)
{-# DEPRECATED cdceDBClusterEndpointIdentifier "Use generic-lens or generic-optics with 'dbClusterEndpointIdentifier' instead." #-}

-- | The type of the endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdceEndpointType :: Lens.Lens' CreateDBClusterEndpoint Lude.Text
cdceEndpointType = Lens.lens (endpointType :: CreateDBClusterEndpoint -> Lude.Text) (\s a -> s {endpointType = a} :: CreateDBClusterEndpoint)
{-# DEPRECATED cdceEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

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
        "StaticMembers"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> staticMembers),
        "ExcludedMembers"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> excludedMembers),
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "DBClusterIdentifier" Lude.=: dbClusterIdentifier,
        "DBClusterEndpointIdentifier" Lude.=: dbClusterEndpointIdentifier,
        "EndpointType" Lude.=: endpointType
      ]
