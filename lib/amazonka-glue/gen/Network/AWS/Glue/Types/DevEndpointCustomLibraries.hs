{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DevEndpointCustomLibraries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DevEndpointCustomLibraries where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Custom libraries to be loaded into a development endpoint.
--
--
--
-- /See:/ 'devEndpointCustomLibraries' smart constructor.
data DevEndpointCustomLibraries = DevEndpointCustomLibraries'
  { _declExtraPythonLibsS3Path ::
      !(Maybe Text),
    _declExtraJARsS3Path :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DevEndpointCustomLibraries' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'declExtraPythonLibsS3Path' - The paths to one or more Python libraries in an Amazon Simple Storage Service (Amazon S3) bucket that should be loaded in your @DevEndpoint@ . Multiple values must be complete paths separated by a comma.
--
-- * 'declExtraJARsS3Path' - The path to one or more Java @.jar@ files in an S3 bucket that should be loaded in your @DevEndpoint@ .
devEndpointCustomLibraries ::
  DevEndpointCustomLibraries
devEndpointCustomLibraries =
  DevEndpointCustomLibraries'
    { _declExtraPythonLibsS3Path = Nothing,
      _declExtraJARsS3Path = Nothing
    }

-- | The paths to one or more Python libraries in an Amazon Simple Storage Service (Amazon S3) bucket that should be loaded in your @DevEndpoint@ . Multiple values must be complete paths separated by a comma.
declExtraPythonLibsS3Path :: Lens' DevEndpointCustomLibraries (Maybe Text)
declExtraPythonLibsS3Path = lens _declExtraPythonLibsS3Path (\s a -> s {_declExtraPythonLibsS3Path = a})

-- | The path to one or more Java @.jar@ files in an S3 bucket that should be loaded in your @DevEndpoint@ .
declExtraJARsS3Path :: Lens' DevEndpointCustomLibraries (Maybe Text)
declExtraJARsS3Path = lens _declExtraJARsS3Path (\s a -> s {_declExtraJARsS3Path = a})

instance Hashable DevEndpointCustomLibraries

instance NFData DevEndpointCustomLibraries

instance ToJSON DevEndpointCustomLibraries where
  toJSON DevEndpointCustomLibraries' {..} =
    object
      ( catMaybes
          [ ("ExtraPythonLibsS3Path" .=) <$> _declExtraPythonLibsS3Path,
            ("ExtraJarsS3Path" .=) <$> _declExtraJARsS3Path
          ]
      )
