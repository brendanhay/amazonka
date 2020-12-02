{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SolutionStackDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SolutionStackDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the solution stack.
--
--
--
-- /See:/ 'solutionStackDescription' smart constructor.
data SolutionStackDescription = SolutionStackDescription'
  { _ssdPermittedFileTypes ::
      !(Maybe [Text]),
    _ssdSolutionStackName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SolutionStackDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssdPermittedFileTypes' - The permitted file types allowed for a solution stack.
--
-- * 'ssdSolutionStackName' - The name of the solution stack.
solutionStackDescription ::
  SolutionStackDescription
solutionStackDescription =
  SolutionStackDescription'
    { _ssdPermittedFileTypes = Nothing,
      _ssdSolutionStackName = Nothing
    }

-- | The permitted file types allowed for a solution stack.
ssdPermittedFileTypes :: Lens' SolutionStackDescription [Text]
ssdPermittedFileTypes = lens _ssdPermittedFileTypes (\s a -> s {_ssdPermittedFileTypes = a}) . _Default . _Coerce

-- | The name of the solution stack.
ssdSolutionStackName :: Lens' SolutionStackDescription (Maybe Text)
ssdSolutionStackName = lens _ssdSolutionStackName (\s a -> s {_ssdSolutionStackName = a})

instance FromXML SolutionStackDescription where
  parseXML x =
    SolutionStackDescription'
      <$> ( x .@? "PermittedFileTypes" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "SolutionStackName")

instance Hashable SolutionStackDescription

instance NFData SolutionStackDescription
