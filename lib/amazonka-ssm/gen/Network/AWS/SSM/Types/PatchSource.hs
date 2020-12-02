{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchSource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the patches to use to update the instances, including target operating systems and source repository. Applies to Linux instances only.
--
--
--
-- /See:/ 'patchSource' smart constructor.
data PatchSource = PatchSource'
  { _psName :: !Text,
    _psProducts :: !(List1 Text),
    _psConfiguration :: !(Sensitive Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'PatchSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psName' - The name specified to identify the patch source.
--
-- * 'psProducts' - The specific operating system versions a patch repository applies to, such as "Ubuntu16.04", "AmazonLinux2016.09", "RedhatEnterpriseLinux7.2" or "Suse12.7". For lists of supported product values, see 'PatchFilter' .
--
-- * 'psConfiguration' - The value of the yum repo configuration. For example: @[main]@  @cachedir=/var/cache/yum/$basesearch$releasever@  @keepcache=0@  @debuglevel=2@
patchSource ::
  -- | 'psName'
  Text ->
  -- | 'psProducts'
  NonEmpty Text ->
  -- | 'psConfiguration'
  Text ->
  PatchSource
patchSource pName_ pProducts_ pConfiguration_ =
  PatchSource'
    { _psName = pName_,
      _psProducts = _List1 # pProducts_,
      _psConfiguration = _Sensitive # pConfiguration_
    }

-- | The name specified to identify the patch source.
psName :: Lens' PatchSource Text
psName = lens _psName (\s a -> s {_psName = a})

-- | The specific operating system versions a patch repository applies to, such as "Ubuntu16.04", "AmazonLinux2016.09", "RedhatEnterpriseLinux7.2" or "Suse12.7". For lists of supported product values, see 'PatchFilter' .
psProducts :: Lens' PatchSource (NonEmpty Text)
psProducts = lens _psProducts (\s a -> s {_psProducts = a}) . _List1

-- | The value of the yum repo configuration. For example: @[main]@  @cachedir=/var/cache/yum/$basesearch$releasever@  @keepcache=0@  @debuglevel=2@
psConfiguration :: Lens' PatchSource Text
psConfiguration = lens _psConfiguration (\s a -> s {_psConfiguration = a}) . _Sensitive

instance FromJSON PatchSource where
  parseJSON =
    withObject
      "PatchSource"
      ( \x ->
          PatchSource'
            <$> (x .: "Name") <*> (x .: "Products") <*> (x .: "Configuration")
      )

instance Hashable PatchSource

instance NFData PatchSource

instance ToJSON PatchSource where
  toJSON PatchSource' {..} =
    object
      ( catMaybes
          [ Just ("Name" .= _psName),
            Just ("Products" .= _psProducts),
            Just ("Configuration" .= _psConfiguration)
          ]
      )
