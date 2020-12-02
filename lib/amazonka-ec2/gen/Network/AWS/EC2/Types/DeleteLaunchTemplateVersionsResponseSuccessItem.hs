{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseSuccessItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseSuccessItem where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a launch template version that was successfully deleted.
--
--
--
-- /See:/ 'deleteLaunchTemplateVersionsResponseSuccessItem' smart constructor.
data DeleteLaunchTemplateVersionsResponseSuccessItem = DeleteLaunchTemplateVersionsResponseSuccessItem'
  { _dltvrsiLaunchTemplateName ::
      !( Maybe
           Text
       ),
    _dltvrsiLaunchTemplateId ::
      !( Maybe
           Text
       ),
    _dltvrsiVersionNumber ::
      !( Maybe
           Integer
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeleteLaunchTemplateVersionsResponseSuccessItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltvrsiLaunchTemplateName' - The name of the launch template.
--
-- * 'dltvrsiLaunchTemplateId' - The ID of the launch template.
--
-- * 'dltvrsiVersionNumber' - The version number of the launch template.
deleteLaunchTemplateVersionsResponseSuccessItem ::
  DeleteLaunchTemplateVersionsResponseSuccessItem
deleteLaunchTemplateVersionsResponseSuccessItem =
  DeleteLaunchTemplateVersionsResponseSuccessItem'
    { _dltvrsiLaunchTemplateName =
        Nothing,
      _dltvrsiLaunchTemplateId = Nothing,
      _dltvrsiVersionNumber = Nothing
    }

-- | The name of the launch template.
dltvrsiLaunchTemplateName :: Lens' DeleteLaunchTemplateVersionsResponseSuccessItem (Maybe Text)
dltvrsiLaunchTemplateName = lens _dltvrsiLaunchTemplateName (\s a -> s {_dltvrsiLaunchTemplateName = a})

-- | The ID of the launch template.
dltvrsiLaunchTemplateId :: Lens' DeleteLaunchTemplateVersionsResponseSuccessItem (Maybe Text)
dltvrsiLaunchTemplateId = lens _dltvrsiLaunchTemplateId (\s a -> s {_dltvrsiLaunchTemplateId = a})

-- | The version number of the launch template.
dltvrsiVersionNumber :: Lens' DeleteLaunchTemplateVersionsResponseSuccessItem (Maybe Integer)
dltvrsiVersionNumber = lens _dltvrsiVersionNumber (\s a -> s {_dltvrsiVersionNumber = a})

instance FromXML DeleteLaunchTemplateVersionsResponseSuccessItem where
  parseXML x =
    DeleteLaunchTemplateVersionsResponseSuccessItem'
      <$> (x .@? "launchTemplateName")
      <*> (x .@? "launchTemplateId")
      <*> (x .@? "versionNumber")

instance Hashable DeleteLaunchTemplateVersionsResponseSuccessItem

instance NFData DeleteLaunchTemplateVersionsResponseSuccessItem
