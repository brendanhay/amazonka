{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.NamedQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.NamedQuery where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A query, where @QueryString@ is the list of SQL query statements that comprise the query.
--
--
--
-- /See:/ 'namedQuery' smart constructor.
data NamedQuery = NamedQuery'
  { _nqNamedQueryId :: !(Maybe Text),
    _nqDescription :: !(Maybe Text),
    _nqWorkGroup :: !(Maybe Text),
    _nqName :: !Text,
    _nqDatabase :: !Text,
    _nqQueryString :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NamedQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nqNamedQueryId' - The unique identifier of the query.
--
-- * 'nqDescription' - The query description.
--
-- * 'nqWorkGroup' - The name of the workgroup that contains the named query.
--
-- * 'nqName' - The query name.
--
-- * 'nqDatabase' - The database to which the query belongs.
--
-- * 'nqQueryString' - The SQL query statements that comprise the query.
namedQuery ::
  -- | 'nqName'
  Text ->
  -- | 'nqDatabase'
  Text ->
  -- | 'nqQueryString'
  Text ->
  NamedQuery
namedQuery pName_ pDatabase_ pQueryString_ =
  NamedQuery'
    { _nqNamedQueryId = Nothing,
      _nqDescription = Nothing,
      _nqWorkGroup = Nothing,
      _nqName = pName_,
      _nqDatabase = pDatabase_,
      _nqQueryString = pQueryString_
    }

-- | The unique identifier of the query.
nqNamedQueryId :: Lens' NamedQuery (Maybe Text)
nqNamedQueryId = lens _nqNamedQueryId (\s a -> s {_nqNamedQueryId = a})

-- | The query description.
nqDescription :: Lens' NamedQuery (Maybe Text)
nqDescription = lens _nqDescription (\s a -> s {_nqDescription = a})

-- | The name of the workgroup that contains the named query.
nqWorkGroup :: Lens' NamedQuery (Maybe Text)
nqWorkGroup = lens _nqWorkGroup (\s a -> s {_nqWorkGroup = a})

-- | The query name.
nqName :: Lens' NamedQuery Text
nqName = lens _nqName (\s a -> s {_nqName = a})

-- | The database to which the query belongs.
nqDatabase :: Lens' NamedQuery Text
nqDatabase = lens _nqDatabase (\s a -> s {_nqDatabase = a})

-- | The SQL query statements that comprise the query.
nqQueryString :: Lens' NamedQuery Text
nqQueryString = lens _nqQueryString (\s a -> s {_nqQueryString = a})

instance FromJSON NamedQuery where
  parseJSON =
    withObject
      "NamedQuery"
      ( \x ->
          NamedQuery'
            <$> (x .:? "NamedQueryId")
            <*> (x .:? "Description")
            <*> (x .:? "WorkGroup")
            <*> (x .: "Name")
            <*> (x .: "Database")
            <*> (x .: "QueryString")
      )

instance Hashable NamedQuery

instance NFData NamedQuery
