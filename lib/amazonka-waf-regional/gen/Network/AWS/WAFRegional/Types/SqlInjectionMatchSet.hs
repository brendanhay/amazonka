{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.SqlInjectionMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.SqlInjectionMatchSet where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAFRegional.Types.SqlInjectionMatchTuple

-- | A complex type that contains @SqlInjectionMatchTuple@ objects, which specify the parts of web requests that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header. If a @SqlInjectionMatchSet@ contains more than one @SqlInjectionMatchTuple@ object, a request needs to include snippets of SQL code in only one of the specified parts of the request to be considered a match.
--
--
--
-- /See:/ 'sqlInjectionMatchSet' smart constructor.
data SqlInjectionMatchSet = SqlInjectionMatchSet'
  { _simsName ::
      !(Maybe Text),
    _simsSqlInjectionMatchSetId :: !Text,
    _simsSqlInjectionMatchTuples ::
      ![SqlInjectionMatchTuple]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SqlInjectionMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'simsName' - The name, if any, of the @SqlInjectionMatchSet@ .
--
-- * 'simsSqlInjectionMatchSetId' - A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ). @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- * 'simsSqlInjectionMatchTuples' - Specifies the parts of web requests that you want to inspect for snippets of malicious SQL code.
sqlInjectionMatchSet ::
  -- | 'simsSqlInjectionMatchSetId'
  Text ->
  SqlInjectionMatchSet
sqlInjectionMatchSet pSqlInjectionMatchSetId_ =
  SqlInjectionMatchSet'
    { _simsName = Nothing,
      _simsSqlInjectionMatchSetId = pSqlInjectionMatchSetId_,
      _simsSqlInjectionMatchTuples = mempty
    }

-- | The name, if any, of the @SqlInjectionMatchSet@ .
simsName :: Lens' SqlInjectionMatchSet (Maybe Text)
simsName = lens _simsName (\s a -> s {_simsName = a})

-- | A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ). @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
simsSqlInjectionMatchSetId :: Lens' SqlInjectionMatchSet Text
simsSqlInjectionMatchSetId = lens _simsSqlInjectionMatchSetId (\s a -> s {_simsSqlInjectionMatchSetId = a})

-- | Specifies the parts of web requests that you want to inspect for snippets of malicious SQL code.
simsSqlInjectionMatchTuples :: Lens' SqlInjectionMatchSet [SqlInjectionMatchTuple]
simsSqlInjectionMatchTuples = lens _simsSqlInjectionMatchTuples (\s a -> s {_simsSqlInjectionMatchTuples = a}) . _Coerce

instance FromJSON SqlInjectionMatchSet where
  parseJSON =
    withObject
      "SqlInjectionMatchSet"
      ( \x ->
          SqlInjectionMatchSet'
            <$> (x .:? "Name")
            <*> (x .: "SqlInjectionMatchSetId")
            <*> (x .:? "SqlInjectionMatchTuples" .!= mempty)
      )

instance Hashable SqlInjectionMatchSet

instance NFData SqlInjectionMatchSet
