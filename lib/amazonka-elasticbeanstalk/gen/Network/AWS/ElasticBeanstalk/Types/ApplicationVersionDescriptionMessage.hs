{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescriptionMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescriptionMessage where

import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Result message wrapping a single description of an application version.
--
--
--
-- /See:/ 'applicationVersionDescriptionMessage' smart constructor.
newtype ApplicationVersionDescriptionMessage = ApplicationVersionDescriptionMessage'
  { _avdmApplicationVersion ::
      Maybe
        ApplicationVersionDescription
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApplicationVersionDescriptionMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avdmApplicationVersion' - The 'ApplicationVersionDescription' of the application version.
applicationVersionDescriptionMessage ::
  ApplicationVersionDescriptionMessage
applicationVersionDescriptionMessage =
  ApplicationVersionDescriptionMessage'
    { _avdmApplicationVersion =
        Nothing
    }

-- | The 'ApplicationVersionDescription' of the application version.
avdmApplicationVersion :: Lens' ApplicationVersionDescriptionMessage (Maybe ApplicationVersionDescription)
avdmApplicationVersion = lens _avdmApplicationVersion (\s a -> s {_avdmApplicationVersion = a})

instance FromXML ApplicationVersionDescriptionMessage where
  parseXML x =
    ApplicationVersionDescriptionMessage'
      <$> (x .@? "ApplicationVersion")

instance Hashable ApplicationVersionDescriptionMessage

instance NFData ApplicationVersionDescriptionMessage
