{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Parent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Parent where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A parent label for a label. A label can have 0, 1, or more parents.
--
--
--
-- /See:/ 'parent' smart constructor.
newtype Parent = Parent' {_pName :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Parent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pName' - The name of the parent label.
parent ::
  Parent
parent = Parent' {_pName = Nothing}

-- | The name of the parent label.
pName :: Lens' Parent (Maybe Text)
pName = lens _pName (\s a -> s {_pName = a})

instance FromJSON Parent where
  parseJSON = withObject "Parent" (\x -> Parent' <$> (x .:? "Name"))

instance Hashable Parent

instance NFData Parent
