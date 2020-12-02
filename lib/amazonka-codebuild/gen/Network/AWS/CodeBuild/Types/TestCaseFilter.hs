{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.TestCaseFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.TestCaseFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A filter used to return specific types of test cases. In order to pass the filter, the report must meet all of the filter properties.
--
--
--
-- /See:/ 'testCaseFilter' smart constructor.
data TestCaseFilter = TestCaseFilter'
  { _tcfStatus :: !(Maybe Text),
    _tcfKeyword :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TestCaseFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcfStatus' - The status used to filter test cases. A @TestCaseFilter@ can have one status. Valid values are:     * @SUCCEEDED@      * @FAILED@      * @ERROR@      * @SKIPPED@      * @UNKNOWN@
--
-- * 'tcfKeyword' - A keyword that is used to filter on the @name@ or the @prefix@ of the test cases. Only test cases where the keyword is a substring of the @name@ or the @prefix@ will be returned.
testCaseFilter ::
  TestCaseFilter
testCaseFilter =
  TestCaseFilter' {_tcfStatus = Nothing, _tcfKeyword = Nothing}

-- | The status used to filter test cases. A @TestCaseFilter@ can have one status. Valid values are:     * @SUCCEEDED@      * @FAILED@      * @ERROR@      * @SKIPPED@      * @UNKNOWN@
tcfStatus :: Lens' TestCaseFilter (Maybe Text)
tcfStatus = lens _tcfStatus (\s a -> s {_tcfStatus = a})

-- | A keyword that is used to filter on the @name@ or the @prefix@ of the test cases. Only test cases where the keyword is a substring of the @name@ or the @prefix@ will be returned.
tcfKeyword :: Lens' TestCaseFilter (Maybe Text)
tcfKeyword = lens _tcfKeyword (\s a -> s {_tcfKeyword = a})

instance Hashable TestCaseFilter

instance NFData TestCaseFilter

instance ToJSON TestCaseFilter where
  toJSON TestCaseFilter' {..} =
    object
      ( catMaybes
          [("status" .=) <$> _tcfStatus, ("keyword" .=) <$> _tcfKeyword]
      )
