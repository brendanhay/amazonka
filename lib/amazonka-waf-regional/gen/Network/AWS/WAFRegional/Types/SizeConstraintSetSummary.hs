{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.SizeConstraintSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.SizeConstraintSetSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The @Id@ and @Name@ of a @SizeConstraintSet@ .
--
--
--
-- /See:/ 'sizeConstraintSetSummary' smart constructor.
data SizeConstraintSetSummary = SizeConstraintSetSummary'
  { _scssSizeConstraintSetId ::
      !Text,
    _scssName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SizeConstraintSetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scssSizeConstraintSetId' - A unique identifier for a @SizeConstraintSet@ . You use @SizeConstraintSetId@ to get information about a @SizeConstraintSet@ (see 'GetSizeConstraintSet' ), update a @SizeConstraintSet@ (see 'UpdateSizeConstraintSet' ), insert a @SizeConstraintSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SizeConstraintSet@ from AWS WAF (see 'DeleteSizeConstraintSet' ). @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
--
-- * 'scssName' - The name of the @SizeConstraintSet@ , if any.
sizeConstraintSetSummary ::
  -- | 'scssSizeConstraintSetId'
  Text ->
  -- | 'scssName'
  Text ->
  SizeConstraintSetSummary
sizeConstraintSetSummary pSizeConstraintSetId_ pName_ =
  SizeConstraintSetSummary'
    { _scssSizeConstraintSetId =
        pSizeConstraintSetId_,
      _scssName = pName_
    }

-- | A unique identifier for a @SizeConstraintSet@ . You use @SizeConstraintSetId@ to get information about a @SizeConstraintSet@ (see 'GetSizeConstraintSet' ), update a @SizeConstraintSet@ (see 'UpdateSizeConstraintSet' ), insert a @SizeConstraintSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SizeConstraintSet@ from AWS WAF (see 'DeleteSizeConstraintSet' ). @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
scssSizeConstraintSetId :: Lens' SizeConstraintSetSummary Text
scssSizeConstraintSetId = lens _scssSizeConstraintSetId (\s a -> s {_scssSizeConstraintSetId = a})

-- | The name of the @SizeConstraintSet@ , if any.
scssName :: Lens' SizeConstraintSetSummary Text
scssName = lens _scssName (\s a -> s {_scssName = a})

instance FromJSON SizeConstraintSetSummary where
  parseJSON =
    withObject
      "SizeConstraintSetSummary"
      ( \x ->
          SizeConstraintSetSummary'
            <$> (x .: "SizeConstraintSetId") <*> (x .: "Name")
      )

instance Hashable SizeConstraintSetSummary

instance NFData SizeConstraintSetSummary
