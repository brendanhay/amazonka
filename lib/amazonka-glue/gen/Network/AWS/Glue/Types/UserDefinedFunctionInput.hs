{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UserDefinedFunctionInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.UserDefinedFunctionInput where

import Network.AWS.Glue.Types.PrincipalType
import Network.AWS.Glue.Types.ResourceURI
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure used to create or update a user-defined function.
--
--
--
-- /See:/ 'userDefinedFunctionInput' smart constructor.
data UserDefinedFunctionInput = UserDefinedFunctionInput'
  { _udfiOwnerName ::
      !(Maybe Text),
    _udfiResourceURIs ::
      !(Maybe [ResourceURI]),
    _udfiFunctionName :: !(Maybe Text),
    _udfiOwnerType :: !(Maybe PrincipalType),
    _udfiClassName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserDefinedFunctionInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udfiOwnerName' - The owner of the function.
--
-- * 'udfiResourceURIs' - The resource URIs for the function.
--
-- * 'udfiFunctionName' - The name of the function.
--
-- * 'udfiOwnerType' - The owner type.
--
-- * 'udfiClassName' - The Java class that contains the function code.
userDefinedFunctionInput ::
  UserDefinedFunctionInput
userDefinedFunctionInput =
  UserDefinedFunctionInput'
    { _udfiOwnerName = Nothing,
      _udfiResourceURIs = Nothing,
      _udfiFunctionName = Nothing,
      _udfiOwnerType = Nothing,
      _udfiClassName = Nothing
    }

-- | The owner of the function.
udfiOwnerName :: Lens' UserDefinedFunctionInput (Maybe Text)
udfiOwnerName = lens _udfiOwnerName (\s a -> s {_udfiOwnerName = a})

-- | The resource URIs for the function.
udfiResourceURIs :: Lens' UserDefinedFunctionInput [ResourceURI]
udfiResourceURIs = lens _udfiResourceURIs (\s a -> s {_udfiResourceURIs = a}) . _Default . _Coerce

-- | The name of the function.
udfiFunctionName :: Lens' UserDefinedFunctionInput (Maybe Text)
udfiFunctionName = lens _udfiFunctionName (\s a -> s {_udfiFunctionName = a})

-- | The owner type.
udfiOwnerType :: Lens' UserDefinedFunctionInput (Maybe PrincipalType)
udfiOwnerType = lens _udfiOwnerType (\s a -> s {_udfiOwnerType = a})

-- | The Java class that contains the function code.
udfiClassName :: Lens' UserDefinedFunctionInput (Maybe Text)
udfiClassName = lens _udfiClassName (\s a -> s {_udfiClassName = a})

instance Hashable UserDefinedFunctionInput

instance NFData UserDefinedFunctionInput

instance ToJSON UserDefinedFunctionInput where
  toJSON UserDefinedFunctionInput' {..} =
    object
      ( catMaybes
          [ ("OwnerName" .=) <$> _udfiOwnerName,
            ("ResourceUris" .=) <$> _udfiResourceURIs,
            ("FunctionName" .=) <$> _udfiFunctionName,
            ("OwnerType" .=) <$> _udfiOwnerType,
            ("ClassName" .=) <$> _udfiClassName
          ]
      )
