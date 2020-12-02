{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceAssociation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | One or more association documents on the instance.
--
--
--
-- /See:/ 'instanceAssociation' smart constructor.
data InstanceAssociation = InstanceAssociation'
  { _iaAssociationId ::
      !(Maybe Text),
    _iaInstanceId :: !(Maybe Text),
    _iaContent :: !(Maybe Text),
    _iaAssociationVersion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaAssociationId' - The association ID.
--
-- * 'iaInstanceId' - The instance ID.
--
-- * 'iaContent' - The content of the association document for the instance(s).
--
-- * 'iaAssociationVersion' - Version information for the association on the instance.
instanceAssociation ::
  InstanceAssociation
instanceAssociation =
  InstanceAssociation'
    { _iaAssociationId = Nothing,
      _iaInstanceId = Nothing,
      _iaContent = Nothing,
      _iaAssociationVersion = Nothing
    }

-- | The association ID.
iaAssociationId :: Lens' InstanceAssociation (Maybe Text)
iaAssociationId = lens _iaAssociationId (\s a -> s {_iaAssociationId = a})

-- | The instance ID.
iaInstanceId :: Lens' InstanceAssociation (Maybe Text)
iaInstanceId = lens _iaInstanceId (\s a -> s {_iaInstanceId = a})

-- | The content of the association document for the instance(s).
iaContent :: Lens' InstanceAssociation (Maybe Text)
iaContent = lens _iaContent (\s a -> s {_iaContent = a})

-- | Version information for the association on the instance.
iaAssociationVersion :: Lens' InstanceAssociation (Maybe Text)
iaAssociationVersion = lens _iaAssociationVersion (\s a -> s {_iaAssociationVersion = a})

instance FromJSON InstanceAssociation where
  parseJSON =
    withObject
      "InstanceAssociation"
      ( \x ->
          InstanceAssociation'
            <$> (x .:? "AssociationId")
            <*> (x .:? "InstanceId")
            <*> (x .:? "Content")
            <*> (x .:? "AssociationVersion")
      )

instance Hashable InstanceAssociation

instance NFData InstanceAssociation
