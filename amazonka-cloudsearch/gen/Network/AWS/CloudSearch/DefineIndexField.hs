{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudSearch.DefineIndexField
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Configures an @IndexField@ for the search domain. Used to create new
-- fields and modify existing ones. You must specify the name of the domain
-- you are configuring and an index field configuration. The index field
-- configuration specifies a unique name, the index field type, and the
-- options you want to configure for the field. The options you can specify
-- depend on the @IndexFieldType@. If the field exists, the new
-- configuration replaces the old one. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DefineIndexField.html>
module Network.AWS.CloudSearch.DefineIndexField
    (
    -- * Request
      DefineIndexField
    -- ** Request constructor
    , defineIndexField
    -- ** Request lenses
    , def1DomainName
    , def1IndexField

    -- * Response
    , DefineIndexFieldResponse
    -- ** Response constructor
    , defineIndexFieldResponse
    -- ** Response lenses
    , defIndexField
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'defineIndexField' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'def1DomainName'
--
-- * 'def1IndexField'
data DefineIndexField = DefineIndexField'{_def1DomainName :: Text, _def1IndexField :: IndexField} deriving (Eq, Read, Show)

-- | 'DefineIndexField' smart constructor.
defineIndexField :: Text -> IndexField -> DefineIndexField
defineIndexField pDomainName pIndexField = DefineIndexField'{_def1DomainName = pDomainName, _def1IndexField = pIndexField};

-- | FIXME: Undocumented member.
def1DomainName :: Lens' DefineIndexField Text
def1DomainName = lens _def1DomainName (\ s a -> s{_def1DomainName = a});

-- | The index field and field options you want to configure.
def1IndexField :: Lens' DefineIndexField IndexField
def1IndexField = lens _def1IndexField (\ s a -> s{_def1IndexField = a});

instance AWSRequest DefineIndexField where
        type Sv DefineIndexField = CloudSearch
        type Rs DefineIndexField = DefineIndexFieldResponse
        request = post
        response
          = receiveXMLWrapper "DefineIndexFieldResult"
              (\ s h x ->
                 DefineIndexFieldResponse' <$> (x .@ "IndexField"))

instance ToHeaders DefineIndexField where
        toHeaders = const mempty

instance ToPath DefineIndexField where
        toPath = const "/"

instance ToQuery DefineIndexField where
        toQuery DefineIndexField'{..}
          = mconcat
              ["Action" =: ("DefineIndexField" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _def1DomainName,
               "IndexField" =: _def1IndexField]

-- | /See:/ 'defineIndexFieldResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'defIndexField'
newtype DefineIndexFieldResponse = DefineIndexFieldResponse'{_defIndexField :: IndexFieldStatus} deriving (Eq, Read, Show)

-- | 'DefineIndexFieldResponse' smart constructor.
defineIndexFieldResponse :: IndexFieldStatus -> DefineIndexFieldResponse
defineIndexFieldResponse pIndexField = DefineIndexFieldResponse'{_defIndexField = pIndexField};

-- | FIXME: Undocumented member.
defIndexField :: Lens' DefineIndexFieldResponse IndexFieldStatus
defIndexField = lens _defIndexField (\ s a -> s{_defIndexField = a});
