{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.NamedQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.NamedQuery
  ( NamedQuery (..),

    -- * Smart constructor
    mkNamedQuery,

    -- * Lenses
    nqNamedQueryId,
    nqDatabase,
    nqName,
    nqQueryString,
    nqDescription,
    nqWorkGroup,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A query, where @QueryString@ is the list of SQL query statements that comprise the query.
--
-- /See:/ 'mkNamedQuery' smart constructor.
data NamedQuery = NamedQuery'
  { -- | The unique identifier of the query.
    namedQueryId :: Lude.Maybe Lude.Text,
    -- | The database to which the query belongs.
    database :: Lude.Text,
    -- | The query name.
    name :: Lude.Text,
    -- | The SQL query statements that comprise the query.
    queryString :: Lude.Text,
    -- | The query description.
    description :: Lude.Maybe Lude.Text,
    -- | The name of the workgroup that contains the named query.
    workGroup :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NamedQuery' with the minimum fields required to make a request.
--
-- * 'namedQueryId' - The unique identifier of the query.
-- * 'database' - The database to which the query belongs.
-- * 'name' - The query name.
-- * 'queryString' - The SQL query statements that comprise the query.
-- * 'description' - The query description.
-- * 'workGroup' - The name of the workgroup that contains the named query.
mkNamedQuery ::
  -- | 'database'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'queryString'
  Lude.Text ->
  NamedQuery
mkNamedQuery pDatabase_ pName_ pQueryString_ =
  NamedQuery'
    { namedQueryId = Lude.Nothing,
      database = pDatabase_,
      name = pName_,
      queryString = pQueryString_,
      description = Lude.Nothing,
      workGroup = Lude.Nothing
    }

-- | The unique identifier of the query.
--
-- /Note:/ Consider using 'namedQueryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nqNamedQueryId :: Lens.Lens' NamedQuery (Lude.Maybe Lude.Text)
nqNamedQueryId = Lens.lens (namedQueryId :: NamedQuery -> Lude.Maybe Lude.Text) (\s a -> s {namedQueryId = a} :: NamedQuery)
{-# DEPRECATED nqNamedQueryId "Use generic-lens or generic-optics with 'namedQueryId' instead." #-}

-- | The database to which the query belongs.
--
-- /Note:/ Consider using 'database' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nqDatabase :: Lens.Lens' NamedQuery Lude.Text
nqDatabase = Lens.lens (database :: NamedQuery -> Lude.Text) (\s a -> s {database = a} :: NamedQuery)
{-# DEPRECATED nqDatabase "Use generic-lens or generic-optics with 'database' instead." #-}

-- | The query name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nqName :: Lens.Lens' NamedQuery Lude.Text
nqName = Lens.lens (name :: NamedQuery -> Lude.Text) (\s a -> s {name = a} :: NamedQuery)
{-# DEPRECATED nqName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The SQL query statements that comprise the query.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nqQueryString :: Lens.Lens' NamedQuery Lude.Text
nqQueryString = Lens.lens (queryString :: NamedQuery -> Lude.Text) (\s a -> s {queryString = a} :: NamedQuery)
{-# DEPRECATED nqQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | The query description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nqDescription :: Lens.Lens' NamedQuery (Lude.Maybe Lude.Text)
nqDescription = Lens.lens (description :: NamedQuery -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: NamedQuery)
{-# DEPRECATED nqDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the workgroup that contains the named query.
--
-- /Note:/ Consider using 'workGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nqWorkGroup :: Lens.Lens' NamedQuery (Lude.Maybe Lude.Text)
nqWorkGroup = Lens.lens (workGroup :: NamedQuery -> Lude.Maybe Lude.Text) (\s a -> s {workGroup = a} :: NamedQuery)
{-# DEPRECATED nqWorkGroup "Use generic-lens or generic-optics with 'workGroup' instead." #-}

instance Lude.FromJSON NamedQuery where
  parseJSON =
    Lude.withObject
      "NamedQuery"
      ( \x ->
          NamedQuery'
            Lude.<$> (x Lude..:? "NamedQueryId")
            Lude.<*> (x Lude..: "Database")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "QueryString")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "WorkGroup")
      )
