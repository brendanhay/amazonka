{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.SqlInjectionMatchSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.SqlInjectionMatchSetSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The @Id@ and @Name@ of a @SqlInjectionMatchSet@ .
--
--
--
-- /See:/ 'sqlInjectionMatchSetSummary' smart constructor.
data SqlInjectionMatchSetSummary = SqlInjectionMatchSetSummary'
  { _simssSqlInjectionMatchSetId ::
      !Text,
    _simssName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SqlInjectionMatchSetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'simssSqlInjectionMatchSetId' - A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ). @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- * 'simssName' - The name of the @SqlInjectionMatchSet@ , if any, specified by @Id@ .
sqlInjectionMatchSetSummary ::
  -- | 'simssSqlInjectionMatchSetId'
  Text ->
  -- | 'simssName'
  Text ->
  SqlInjectionMatchSetSummary
sqlInjectionMatchSetSummary pSqlInjectionMatchSetId_ pName_ =
  SqlInjectionMatchSetSummary'
    { _simssSqlInjectionMatchSetId =
        pSqlInjectionMatchSetId_,
      _simssName = pName_
    }

-- | A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ). @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
simssSqlInjectionMatchSetId :: Lens' SqlInjectionMatchSetSummary Text
simssSqlInjectionMatchSetId = lens _simssSqlInjectionMatchSetId (\s a -> s {_simssSqlInjectionMatchSetId = a})

-- | The name of the @SqlInjectionMatchSet@ , if any, specified by @Id@ .
simssName :: Lens' SqlInjectionMatchSetSummary Text
simssName = lens _simssName (\s a -> s {_simssName = a})

instance FromJSON SqlInjectionMatchSetSummary where
  parseJSON =
    withObject
      "SqlInjectionMatchSetSummary"
      ( \x ->
          SqlInjectionMatchSetSummary'
            <$> (x .: "SqlInjectionMatchSetId") <*> (x .: "Name")
      )

instance Hashable SqlInjectionMatchSetSummary

instance NFData SqlInjectionMatchSetSummary
