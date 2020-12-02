{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationDescriptionMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationDescriptionMessage where

import Network.AWS.ElasticBeanstalk.Types.ApplicationDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Result message containing a single description of an application.
--
--
--
-- /See:/ 'applicationDescriptionMessage' smart constructor.
newtype ApplicationDescriptionMessage = ApplicationDescriptionMessage'
  { _admApplication ::
      Maybe ApplicationDescription
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ApplicationDescriptionMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'admApplication' - The 'ApplicationDescription' of the application.
applicationDescriptionMessage ::
  ApplicationDescriptionMessage
applicationDescriptionMessage =
  ApplicationDescriptionMessage' {_admApplication = Nothing}

-- | The 'ApplicationDescription' of the application.
admApplication :: Lens' ApplicationDescriptionMessage (Maybe ApplicationDescription)
admApplication = lens _admApplication (\s a -> s {_admApplication = a})

instance FromXML ApplicationDescriptionMessage where
  parseXML x =
    ApplicationDescriptionMessage' <$> (x .@? "Application")

instance Hashable ApplicationDescriptionMessage

instance NFData ApplicationDescriptionMessage
