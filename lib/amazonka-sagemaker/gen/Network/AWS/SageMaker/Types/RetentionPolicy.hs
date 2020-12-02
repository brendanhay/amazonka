{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RetentionPolicy where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.RetentionType

-- | The retention policy for data stored on an Amazon Elastic File System (EFS) volume.
--
--
--
-- /See:/ 'retentionPolicy' smart constructor.
newtype RetentionPolicy = RetentionPolicy'
  { _rpHomeEfsFileSystem ::
      Maybe RetentionType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RetentionPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpHomeEfsFileSystem' - The default is @Retain@ , which specifies to keep the data stored on the EFS volume. Specify @Delete@ to delete the data stored on the EFS volume.
retentionPolicy ::
  RetentionPolicy
retentionPolicy = RetentionPolicy' {_rpHomeEfsFileSystem = Nothing}

-- | The default is @Retain@ , which specifies to keep the data stored on the EFS volume. Specify @Delete@ to delete the data stored on the EFS volume.
rpHomeEfsFileSystem :: Lens' RetentionPolicy (Maybe RetentionType)
rpHomeEfsFileSystem = lens _rpHomeEfsFileSystem (\s a -> s {_rpHomeEfsFileSystem = a})

instance Hashable RetentionPolicy

instance NFData RetentionPolicy

instance ToJSON RetentionPolicy where
  toJSON RetentionPolicy' {..} =
    object
      (catMaybes [("HomeEfsFileSystem" .=) <$> _rpHomeEfsFileSystem])
