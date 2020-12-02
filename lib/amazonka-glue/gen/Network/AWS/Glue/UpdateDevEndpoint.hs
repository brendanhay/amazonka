{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateDevEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified development endpoint.
module Network.AWS.Glue.UpdateDevEndpoint
  ( -- * Creating a Request
    updateDevEndpoint,
    UpdateDevEndpoint,

    -- * Request Lenses
    udeAddPublicKeys,
    udeCustomLibraries,
    udePublicKey,
    udeDeleteArguments,
    udeDeletePublicKeys,
    udeUpdateEtlLibraries,
    udeAddArguments,
    udeEndpointName,

    -- * Destructuring the Response
    updateDevEndpointResponse,
    UpdateDevEndpointResponse,

    -- * Response Lenses
    udersResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDevEndpoint' smart constructor.
data UpdateDevEndpoint = UpdateDevEndpoint'
  { _udeAddPublicKeys ::
      !(Maybe [Text]),
    _udeCustomLibraries ::
      !(Maybe DevEndpointCustomLibraries),
    _udePublicKey :: !(Maybe Text),
    _udeDeleteArguments :: !(Maybe [Text]),
    _udeDeletePublicKeys :: !(Maybe [Text]),
    _udeUpdateEtlLibraries :: !(Maybe Bool),
    _udeAddArguments :: !(Maybe (Map Text (Text))),
    _udeEndpointName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDevEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udeAddPublicKeys' - The list of public keys for the @DevEndpoint@ to use.
--
-- * 'udeCustomLibraries' - Custom Python or Java libraries to be loaded in the @DevEndpoint@ .
--
-- * 'udePublicKey' - The public key for the @DevEndpoint@ to use.
--
-- * 'udeDeleteArguments' - The list of argument keys to be deleted from the map of arguments used to configure the @DevEndpoint@ .
--
-- * 'udeDeletePublicKeys' - The list of public keys to be deleted from the @DevEndpoint@ .
--
-- * 'udeUpdateEtlLibraries' - @True@ if the list of custom libraries to be loaded in the development endpoint needs to be updated, or @False@ if otherwise.
--
-- * 'udeAddArguments' - The map of arguments to add the map of arguments used to configure the @DevEndpoint@ . Valid arguments are:     * @"--enable-glue-datacatalog": ""@      * @"GLUE_PYTHON_VERSION": "3"@      * @"GLUE_PYTHON_VERSION": "2"@  You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
--
-- * 'udeEndpointName' - The name of the @DevEndpoint@ to be updated.
updateDevEndpoint ::
  -- | 'udeEndpointName'
  Text ->
  UpdateDevEndpoint
updateDevEndpoint pEndpointName_ =
  UpdateDevEndpoint'
    { _udeAddPublicKeys = Nothing,
      _udeCustomLibraries = Nothing,
      _udePublicKey = Nothing,
      _udeDeleteArguments = Nothing,
      _udeDeletePublicKeys = Nothing,
      _udeUpdateEtlLibraries = Nothing,
      _udeAddArguments = Nothing,
      _udeEndpointName = pEndpointName_
    }

-- | The list of public keys for the @DevEndpoint@ to use.
udeAddPublicKeys :: Lens' UpdateDevEndpoint [Text]
udeAddPublicKeys = lens _udeAddPublicKeys (\s a -> s {_udeAddPublicKeys = a}) . _Default . _Coerce

-- | Custom Python or Java libraries to be loaded in the @DevEndpoint@ .
udeCustomLibraries :: Lens' UpdateDevEndpoint (Maybe DevEndpointCustomLibraries)
udeCustomLibraries = lens _udeCustomLibraries (\s a -> s {_udeCustomLibraries = a})

-- | The public key for the @DevEndpoint@ to use.
udePublicKey :: Lens' UpdateDevEndpoint (Maybe Text)
udePublicKey = lens _udePublicKey (\s a -> s {_udePublicKey = a})

-- | The list of argument keys to be deleted from the map of arguments used to configure the @DevEndpoint@ .
udeDeleteArguments :: Lens' UpdateDevEndpoint [Text]
udeDeleteArguments = lens _udeDeleteArguments (\s a -> s {_udeDeleteArguments = a}) . _Default . _Coerce

-- | The list of public keys to be deleted from the @DevEndpoint@ .
udeDeletePublicKeys :: Lens' UpdateDevEndpoint [Text]
udeDeletePublicKeys = lens _udeDeletePublicKeys (\s a -> s {_udeDeletePublicKeys = a}) . _Default . _Coerce

-- | @True@ if the list of custom libraries to be loaded in the development endpoint needs to be updated, or @False@ if otherwise.
udeUpdateEtlLibraries :: Lens' UpdateDevEndpoint (Maybe Bool)
udeUpdateEtlLibraries = lens _udeUpdateEtlLibraries (\s a -> s {_udeUpdateEtlLibraries = a})

-- | The map of arguments to add the map of arguments used to configure the @DevEndpoint@ . Valid arguments are:     * @"--enable-glue-datacatalog": ""@      * @"GLUE_PYTHON_VERSION": "3"@      * @"GLUE_PYTHON_VERSION": "2"@  You can specify a version of Python support for development endpoints by using the @Arguments@ parameter in the @CreateDevEndpoint@ or @UpdateDevEndpoint@ APIs. If no arguments are provided, the version defaults to Python 2.
udeAddArguments :: Lens' UpdateDevEndpoint (HashMap Text (Text))
udeAddArguments = lens _udeAddArguments (\s a -> s {_udeAddArguments = a}) . _Default . _Map

-- | The name of the @DevEndpoint@ to be updated.
udeEndpointName :: Lens' UpdateDevEndpoint Text
udeEndpointName = lens _udeEndpointName (\s a -> s {_udeEndpointName = a})

instance AWSRequest UpdateDevEndpoint where
  type Rs UpdateDevEndpoint = UpdateDevEndpointResponse
  request = postJSON glue
  response =
    receiveEmpty
      (\s h x -> UpdateDevEndpointResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateDevEndpoint

instance NFData UpdateDevEndpoint

instance ToHeaders UpdateDevEndpoint where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.UpdateDevEndpoint" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateDevEndpoint where
  toJSON UpdateDevEndpoint' {..} =
    object
      ( catMaybes
          [ ("AddPublicKeys" .=) <$> _udeAddPublicKeys,
            ("CustomLibraries" .=) <$> _udeCustomLibraries,
            ("PublicKey" .=) <$> _udePublicKey,
            ("DeleteArguments" .=) <$> _udeDeleteArguments,
            ("DeletePublicKeys" .=) <$> _udeDeletePublicKeys,
            ("UpdateEtlLibraries" .=) <$> _udeUpdateEtlLibraries,
            ("AddArguments" .=) <$> _udeAddArguments,
            Just ("EndpointName" .= _udeEndpointName)
          ]
      )

instance ToPath UpdateDevEndpoint where
  toPath = const "/"

instance ToQuery UpdateDevEndpoint where
  toQuery = const mempty

-- | /See:/ 'updateDevEndpointResponse' smart constructor.
newtype UpdateDevEndpointResponse = UpdateDevEndpointResponse'
  { _udersResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDevEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udersResponseStatus' - -- | The response status code.
updateDevEndpointResponse ::
  -- | 'udersResponseStatus'
  Int ->
  UpdateDevEndpointResponse
updateDevEndpointResponse pResponseStatus_ =
  UpdateDevEndpointResponse'
    { _udersResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
udersResponseStatus :: Lens' UpdateDevEndpointResponse Int
udersResponseStatus = lens _udersResponseStatus (\s a -> s {_udersResponseStatus = a})

instance NFData UpdateDevEndpointResponse
