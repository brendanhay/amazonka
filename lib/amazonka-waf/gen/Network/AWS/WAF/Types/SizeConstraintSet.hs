{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.SizeConstraintSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.SizeConstraintSet where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.SizeConstraint

-- | A complex type that contains @SizeConstraint@ objects, which specify the parts of web requests that you want AWS WAF to inspect the size of. If a @SizeConstraintSet@ contains more than one @SizeConstraint@ object, a request only needs to match one constraint to be considered a match.
--
--
--
-- /See:/ 'sizeConstraintSet' smart constructor.
data SizeConstraintSet = SizeConstraintSet'
  { _scsName ::
      !(Maybe Text),
    _scsSizeConstraintSetId :: !Text,
    _scsSizeConstraints :: ![SizeConstraint]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SizeConstraintSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scsName' - The name, if any, of the @SizeConstraintSet@ .
--
-- * 'scsSizeConstraintSetId' - A unique identifier for a @SizeConstraintSet@ . You use @SizeConstraintSetId@ to get information about a @SizeConstraintSet@ (see 'GetSizeConstraintSet' ), update a @SizeConstraintSet@ (see 'UpdateSizeConstraintSet' ), insert a @SizeConstraintSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SizeConstraintSet@ from AWS WAF (see 'DeleteSizeConstraintSet' ). @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
--
-- * 'scsSizeConstraints' - Specifies the parts of web requests that you want to inspect the size of.
sizeConstraintSet ::
  -- | 'scsSizeConstraintSetId'
  Text ->
  SizeConstraintSet
sizeConstraintSet pSizeConstraintSetId_ =
  SizeConstraintSet'
    { _scsName = Nothing,
      _scsSizeConstraintSetId = pSizeConstraintSetId_,
      _scsSizeConstraints = mempty
    }

-- | The name, if any, of the @SizeConstraintSet@ .
scsName :: Lens' SizeConstraintSet (Maybe Text)
scsName = lens _scsName (\s a -> s {_scsName = a})

-- | A unique identifier for a @SizeConstraintSet@ . You use @SizeConstraintSetId@ to get information about a @SizeConstraintSet@ (see 'GetSizeConstraintSet' ), update a @SizeConstraintSet@ (see 'UpdateSizeConstraintSet' ), insert a @SizeConstraintSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SizeConstraintSet@ from AWS WAF (see 'DeleteSizeConstraintSet' ). @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
scsSizeConstraintSetId :: Lens' SizeConstraintSet Text
scsSizeConstraintSetId = lens _scsSizeConstraintSetId (\s a -> s {_scsSizeConstraintSetId = a})

-- | Specifies the parts of web requests that you want to inspect the size of.
scsSizeConstraints :: Lens' SizeConstraintSet [SizeConstraint]
scsSizeConstraints = lens _scsSizeConstraints (\s a -> s {_scsSizeConstraints = a}) . _Coerce

instance FromJSON SizeConstraintSet where
  parseJSON =
    withObject
      "SizeConstraintSet"
      ( \x ->
          SizeConstraintSet'
            <$> (x .:? "Name")
            <*> (x .: "SizeConstraintSetId")
            <*> (x .:? "SizeConstraints" .!= mempty)
      )

instance Hashable SizeConstraintSet

instance NFData SizeConstraintSet
