{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterEndpoint
  ( DBClusterEndpoint (..),

    -- * Smart constructor
    mkDBClusterEndpoint,

    -- * Lenses
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

-- | This data type represents the information you need to connect to an Amazon Aurora DB cluster. This data type is used as a response element in the following actions:
--
--
--     * @CreateDBClusterEndpoint@
--
--
--     * @DescribeDBClusterEndpoints@
--
--
--     * @ModifyDBClusterEndpoint@
--
--
--     * @DeleteDBClusterEndpoint@
--
--
-- For the data structure that represents Amazon RDS DB instance endpoints, see @Endpoint@ .
--
-- /See:/ 'mkDBClusterEndpoint' smart constructor.
data DBClusterEndpoint = DBClusterEndpoint'
  { -- | The current status of the endpoint. One of: @creating@ , @available@ , @deleting@ , @inactive@ , @modifying@ . The @inactive@ state applies to an endpoint that can't be used for a certain kind of cluster, such as a @writer@ endpoint for a read-only secondary cluster in a global database.
    status :: Lude.Maybe Lude.Text,
    -- | The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
    dbClusterIdentifier :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) for the endpoint.
    dbClusterEndpointARN :: Lude.Maybe Lude.Text,
    -- | The type associated with a custom endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
    customEndpointType :: Lude.Maybe Lude.Text,
    -- | List of DB instance identifiers that are part of the custom endpoint group.
    staticMembers :: Lude.Maybe [Lude.Text],
    -- | The type of the endpoint. One of: @READER@ , @WRITER@ , @CUSTOM@ .
    endpointType :: Lude.Maybe Lude.Text,
    -- | The identifier associated with the endpoint. This parameter is stored as a lowercase string.
    dbClusterEndpointIdentifier :: Lude.Maybe Lude.Text,
    -- | The DNS address of the endpoint.
    endpoint :: Lude.Maybe Lude.Text,
    -- | A unique system-generated identifier for an endpoint. It remains the same for the whole life of the endpoint.
    dbClusterEndpointResourceIdentifier :: Lude.Maybe Lude.Text,
    -- | List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
    excludedMembers :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBClusterEndpoint' with the minimum fields required to make a request.
--
-- * 'status' - The current status of the endpoint. One of: @creating@ , @available@ , @deleting@ , @inactive@ , @modifying@ . The @inactive@ state applies to an endpoint that can't be used for a certain kind of cluster, such as a @writer@ endpoint for a read-only secondary cluster in a global database.
-- * 'dbClusterIdentifier' - The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
-- * 'dbClusterEndpointARN' - The Amazon Resource Name (ARN) for the endpoint.
-- * 'customEndpointType' - The type associated with a custom endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
-- * 'staticMembers' - List of DB instance identifiers that are part of the custom endpoint group.
-- * 'endpointType' - The type of the endpoint. One of: @READER@ , @WRITER@ , @CUSTOM@ .
-- * 'dbClusterEndpointIdentifier' - The identifier associated with the endpoint. This parameter is stored as a lowercase string.
-- * 'endpoint' - The DNS address of the endpoint.
-- * 'dbClusterEndpointResourceIdentifier' - A unique system-generated identifier for an endpoint. It remains the same for the whole life of the endpoint.
-- * 'excludedMembers' - List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
mkDBClusterEndpoint ::
  DBClusterEndpoint
mkDBClusterEndpoint =
  DBClusterEndpoint'
    { status = Lude.Nothing,
      dbClusterIdentifier = Lude.Nothing,
      dbClusterEndpointARN = Lude.Nothing,
      customEndpointType = Lude.Nothing,
      staticMembers = Lude.Nothing,
      endpointType = Lude.Nothing,
      dbClusterEndpointIdentifier = Lude.Nothing,
      endpoint = Lude.Nothing,
      dbClusterEndpointResourceIdentifier = Lude.Nothing,
      excludedMembers = Lude.Nothing
    }

-- | The current status of the endpoint. One of: @creating@ , @available@ , @deleting@ , @inactive@ , @modifying@ . The @inactive@ state applies to an endpoint that can't be used for a certain kind of cluster, such as a @writer@ endpoint for a read-only secondary cluster in a global database.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceStatus :: Lens.Lens' DBClusterEndpoint (Lude.Maybe Lude.Text)
dceStatus = Lens.lens (status :: DBClusterEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: DBClusterEndpoint)
{-# DEPRECATED dceStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The DB cluster identifier of the DB cluster associated with the endpoint. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceDBClusterIdentifier :: Lens.Lens' DBClusterEndpoint (Lude.Maybe Lude.Text)
dceDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: DBClusterEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: DBClusterEndpoint)
{-# DEPRECATED dceDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | The Amazon Resource Name (ARN) for the endpoint.
--
-- /Note:/ Consider using 'dbClusterEndpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceDBClusterEndpointARN :: Lens.Lens' DBClusterEndpoint (Lude.Maybe Lude.Text)
dceDBClusterEndpointARN = Lens.lens (dbClusterEndpointARN :: DBClusterEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterEndpointARN = a} :: DBClusterEndpoint)
{-# DEPRECATED dceDBClusterEndpointARN "Use generic-lens or generic-optics with 'dbClusterEndpointARN' instead." #-}

-- | The type associated with a custom endpoint. One of: @READER@ , @WRITER@ , @ANY@ .
--
-- /Note:/ Consider using 'customEndpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceCustomEndpointType :: Lens.Lens' DBClusterEndpoint (Lude.Maybe Lude.Text)
dceCustomEndpointType = Lens.lens (customEndpointType :: DBClusterEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {customEndpointType = a} :: DBClusterEndpoint)
{-# DEPRECATED dceCustomEndpointType "Use generic-lens or generic-optics with 'customEndpointType' instead." #-}

-- | List of DB instance identifiers that are part of the custom endpoint group.
--
-- /Note:/ Consider using 'staticMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceStaticMembers :: Lens.Lens' DBClusterEndpoint (Lude.Maybe [Lude.Text])
dceStaticMembers = Lens.lens (staticMembers :: DBClusterEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {staticMembers = a} :: DBClusterEndpoint)
{-# DEPRECATED dceStaticMembers "Use generic-lens or generic-optics with 'staticMembers' instead." #-}

-- | The type of the endpoint. One of: @READER@ , @WRITER@ , @CUSTOM@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceEndpointType :: Lens.Lens' DBClusterEndpoint (Lude.Maybe Lude.Text)
dceEndpointType = Lens.lens (endpointType :: DBClusterEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {endpointType = a} :: DBClusterEndpoint)
{-# DEPRECATED dceEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

-- | The identifier associated with the endpoint. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dbClusterEndpointIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceDBClusterEndpointIdentifier :: Lens.Lens' DBClusterEndpoint (Lude.Maybe Lude.Text)
dceDBClusterEndpointIdentifier = Lens.lens (dbClusterEndpointIdentifier :: DBClusterEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterEndpointIdentifier = a} :: DBClusterEndpoint)
{-# DEPRECATED dceDBClusterEndpointIdentifier "Use generic-lens or generic-optics with 'dbClusterEndpointIdentifier' instead." #-}

-- | The DNS address of the endpoint.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceEndpoint :: Lens.Lens' DBClusterEndpoint (Lude.Maybe Lude.Text)
dceEndpoint = Lens.lens (endpoint :: DBClusterEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {endpoint = a} :: DBClusterEndpoint)
{-# DEPRECATED dceEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | A unique system-generated identifier for an endpoint. It remains the same for the whole life of the endpoint.
--
-- /Note:/ Consider using 'dbClusterEndpointResourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceDBClusterEndpointResourceIdentifier :: Lens.Lens' DBClusterEndpoint (Lude.Maybe Lude.Text)
dceDBClusterEndpointResourceIdentifier = Lens.lens (dbClusterEndpointResourceIdentifier :: DBClusterEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterEndpointResourceIdentifier = a} :: DBClusterEndpoint)
{-# DEPRECATED dceDBClusterEndpointResourceIdentifier "Use generic-lens or generic-optics with 'dbClusterEndpointResourceIdentifier' instead." #-}

-- | List of DB instance identifiers that aren't part of the custom endpoint group. All other eligible instances are reachable through the custom endpoint. Only relevant if the list of static members is empty.
--
-- /Note:/ Consider using 'excludedMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceExcludedMembers :: Lens.Lens' DBClusterEndpoint (Lude.Maybe [Lude.Text])
dceExcludedMembers = Lens.lens (excludedMembers :: DBClusterEndpoint -> Lude.Maybe [Lude.Text]) (\s a -> s {excludedMembers = a} :: DBClusterEndpoint)
{-# DEPRECATED dceExcludedMembers "Use generic-lens or generic-optics with 'excludedMembers' instead." #-}

instance Lude.FromXML DBClusterEndpoint where
  parseXML x =
    DBClusterEndpoint'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "DBClusterIdentifier")
      Lude.<*> (x Lude..@? "DBClusterEndpointArn")
      Lude.<*> (x Lude..@? "CustomEndpointType")
      Lude.<*> ( x Lude..@? "StaticMembers" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "EndpointType")
      Lude.<*> (x Lude..@? "DBClusterEndpointIdentifier")
      Lude.<*> (x Lude..@? "Endpoint")
      Lude.<*> (x Lude..@? "DBClusterEndpointResourceIdentifier")
      Lude.<*> ( x Lude..@? "ExcludedMembers" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
